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

import Application
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
  if rat > 1
     then (desirw, floor $ fI desirw * (1 / rat))
     else (floor $ fI desirh * rat, desirh)
  where fI = fromIntegral
        rat = fI origw / fI origh

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
