{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module State.Image where

import Control.Applicative
import Snap.Snaplet
import Control.Monad.Trans (liftIO)
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.FromField
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Configurator as C
import Data.List (sortBy)
import Data.Ord (comparing)
import Graphics.GD hiding (newImage, Image)
import qualified Graphics.GD as GD

import Application
import State.Site
import Helpers.State
import Helpers.Misc
import Helpers.Text

data Image = Image {  imageId :: Int
                    , imageSiteId :: Int
                    , imageSalt :: Text
                    , imageExtension :: Text
                    , imageFormats :: [(Int, (Int, Int))]
                   ,  imageOrigWidth :: Int
                   ,  imageOrigHeight :: Int
                   }

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromField [(Int, (Int, Int))] where
  fromField f Nothing  = returnError ConversionFailed f "data doesn't look readable"
  fromField f (Just s) = maybe (returnError ConversionFailed f "data doesn't look readable")
                               pure
                               (readSafe (B8.unpack s))


standardSizes :: [(Int, (Int, Int))]
standardSizes = [(0, (100,100))
                ,(1, (200,200))
                ,(2, (500, 500))
                ,(3, (1000, 1000))
                ,(4, (9999, 9999))
                ]

buildImagePath :: Image -> Text -> Int -> FilePath
buildImagePath image repo k = (T.unpack repo) ++ (buildImagePart image k)

buildImageUrl :: Image  -> Int -> FilePath
buildImageUrl image k = "/images" ++ (buildImagePart image k)

buildImagePart :: Image -> Int -> FilePath
buildImagePart image k = "/" ++ (show (imageId image)) ++ "_" ++ (T.unpack (imageSalt image)) ++ "_" ++ (show k) ++ ".png"

getImageSizePath :: Image -> Int -> Int -> Text
getImageSizePath im w h =
  let formats = sortBy (comparing fst) (imageFormats im)
      selection = dropWhile (\(_, (w', h')) -> w' < w || h' < h) formats
      (k, _) = if null selection
                then last formats
                else head selection in
  T.pack $ buildImageUrl im k

fixSizes :: Int -> Int -> Int -> Int -> (Int, Int)
fixSizes origw origh desirw desirh =
  -- NOTE(dbp 2014-07-09): This is somewhat tricky. We want to use desire as a
  -- bounding box for an image with the correct ratio as orig.
  if ratDesir < ratOrig -- means desir is too tall, so width is bound
     then (desirw, floor $ fI desirw * (1 / ratOrig))
     else (floor $ fI desirh * ratOrig, desirh)
  where fI = fromIntegral
        ratOrig = fI origw / fI origh
        ratDesir = fI desirw / fI desirh

newImage :: Int -> AppHandler (Maybe Image)
newImage site_id = singleQuery "insert into images (site_id, extension, formats, original_width, original_height) values (?, '', '[]', -1, -1) returning id, site_id, salt, extension, formats, original_width, original_height" (Only site_id)

updateImage :: Image -> AppHandler ()
updateImage (Image i si slt ext fmts ow oh) = void $ execute "update images set site_id = ?, salt = ?, extension = ?, formats = ?, original_width = ?, original_height = ? where id = ?" (si, slt, ext, show fmts, ow, oh, i)

getImageById :: Int -> AppHandler (Maybe Image)
getImageById i = singleQuery "select id, site_id, salt, extension, formats, original_width, original_height from images where id = ?" (Only i)


getImageRepository :: AppHandler Text
getImageRepository = do
  config <- getSnapletUserConfig
  dir <- liftIO $ C.lookup config "imageRepo"
  case dir of
    Nothing -> error "Could not get imageRepo from config."
    Just path -> return path


storeImage :: Site -> Text -> AppHandler Image
storeImage site path = do
  im <- newImage (siteId site)
  case im of
     Nothing -> error "Could not create image."
     Just image -> do
       repo <- getImageRepository
       let lf = loadFileSmart path
       case lf of
         Nothing -> error "Cannot support non-jpg or png"
         Just loadFile -> do
           file <- liftIO (loadFile $ T.unpack path)
           (width, height) <- liftIO (imageSize file)
           makeSizes file image width height repo standardSizes
           updateImage image { imageFormats = standardSizes
                             , imageExtension = "png"
                             , imageOrigWidth = width
                             , imageOrigHeight = height }
           return image
  where loadFileSmart path = if T.isSuffixOf ".jpg" path
                                then Just loadJpegFile
                                else if T.isSuffixOf ".png" path
                                        then Just loadPngFile
                                        else Nothing
        getExtension = T.pack . reverse . (takeWhile (/= '.')) . reverse

makeSizes :: GD.Image -> Image -> Int -> Int ->  Text -> [(Int, (Int, Int))] -> AppHandler ()
makeSizes _    _   _    _    _    []                    = return ()
makeSizes file img maxw maxh repo ((i, (wid, heig)):xs) = do
 if maxw > wid && maxh > heig
    then let (neww, newh) = fixSizes maxw maxh wid heig
         in liftIO $ resizeImage neww newh file >>= savePngFile (buildImagePath img repo i)
    else liftIO $ savePngFile (buildImagePath img repo i) file
 makeSizes file img maxw maxh repo xs
