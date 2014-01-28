{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections  #-}

module Handler.API where

import Prelude hiding (lookup)
import Control.Applicative
import Data.Map (fromList, lookup, insert, (!))
import Data.Monoid
import Snap.Core
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Util.FileUploads
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.Either
import Text.XmlHtml hiding (render)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.Digestive
import Text.Digestive.Snap hiding (method)
import Text.Digestive.Heist
import Graphics.GD hiding (newImage, Image)
import qualified Graphics.GD as GD
import Data.List (isSuffixOf)

import Application
import State.Site
import State.Page
import State.Data
import State.Image
import State.User
import Splice.Data
import Splice.Page
import Helpers.Forms
import Helpers.Misc
import Helpers.Text

-- | Routes for the sites API

siteApiHandler :: Site -> SiteUser -> AppHandler ()
siteApiHandler site user =
  route [("/new/:id", apiId $ apiNewItem site user)
        ,("/delete/:id", apiId $ apiDeleteItem site user)
        ,("/set/:id/image/:field", apiIdFieldItem site (apiSetImageField user))
        ,("/set/:id/:field", apiIdField $ apiSetFieldItem site user)
        ,("/list/:id/:field/add", apiList site (apiListAddItem user))
        ,("/list/:id/:field/delete/:index", apiListIndex site (apiListDeleteItem user))
        ,("/list/:id/:field/set/:index", apiListIndex site (apiListSetItem user))
        ,("/delete/:id/data/:field", apiIdFieldItem site (apiDeleteDataField user))
        ,("/set/:id/data/:field/existing", apiIdFieldItem site (apiSetDataFieldExisting user))
        ,("/set/:id/data/:field/new", apiIdFieldItem site (apiSetDataFieldNew user))
        ,("/list/:id/:field/add/data/existing", apiList site (apiListAddDataExisting user))
        ,("/list/:id/:field/add/data/new", apiList site (apiListAddDataNew user))
        ,("/list/:id/:field/set/:index/data/existing",
         apiListIndex site (apiListSetDataExisting user))
        ,("/list/:id/:field/set/:index/data/new", apiListIndex site (apiListSetDataNew user))
        ]

-- | Helpers for looking up common parameters and loading data

apiId :: (Int -> AppHandler ()) -> AppHandler ()
apiId hndlr = do
  mid <- getParam "id"
  case bsId mid of
     Nothing -> passLog ["Mising id param, or not int."]
     Just id' -> hndlr id'

apiIdField :: (Int -> Text -> AppHandler ()) -> AppHandler ()
apiIdField hndlr = do
  mid <- getParam "id"
  case bsId mid of
     Nothing -> passLog ["Missing id param, or not int."]
     Just id' -> do
       mfld <- getParam "field"
       case fmap decodeUtf8 mfld of
           Nothing -> passLog ["Missing field param."]
           Just field -> hndlr id' field

apiIdFieldIndex :: (Int -> Text -> Int -> AppHandler ()) -> AppHandler ()
apiIdFieldIndex hndlr = do
  mid <- getParam "id"
  case bsId mid of
     Nothing -> passLog ["Missing id param, or not int."]
     Just id' -> do
       mfld <- getParam "field"
       case fmap decodeUtf8 mfld of
           Nothing -> passLog ["Missing field param."]
           Just field -> do
             midx <- getParam "index"
             case bsId midx of
               Nothing -> passLog ["Missing index param"]
               Just idx -> hndlr id' field idx

itemDataFieldSpecLookup :: Site
                        -> Int
                        -> Text
                        -> (Item -> Data -> FieldSpec -> AppHandler ())
                        -> AppHandler ()
itemDataFieldSpecLookup site item_id field hndlr = do
   mit <- getItemById site item_id
   case mit of
     Nothing -> passLog ["No item with id ", (tshow item_id)]
     Just item -> do
       d <- getDataById site (itemDataId item)
       case d of
         Nothing -> passLog ["Data id in item not valid: ", tshow (itemDataId item)]
         Just dat ->
           case lookup field (dataFields dat) of
             Nothing -> passLog ["Field missing from data: ", field]
             Just spec -> hndlr item dat spec



apiIdFieldItem :: Site -> (Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()) -> AppHandler ()
apiIdFieldItem site hndlr = apiIdField $ \item_id field -> itemDataFieldSpecLookup site item_id field (hndlr site item_id field)

apiList :: Site -> (Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()) -> AppHandler ()
apiList site hndlr = apiIdFieldItem site $ \_site item_id field item dat spec' ->
  case spec' of
    ListFieldSpec spec -> hndlr site item_id field item dat spec
    _ -> passLog ["Tried to use a non-list field with list API endpoints."]

apiListIndex :: Site -> (Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()) -> AppHandler ()
apiListIndex site hndlr = apiIdFieldIndex $ \item_id field index ->
  itemDataFieldSpecLookup site item_id field $ \item dat spec ->
    case spec of
      ListFieldSpec spec' -> hndlr site item_id field index item dat spec'
      _ -> passLog ["Tried to use a non-list field with list API endpoints."]

authcheck :: SiteUser -> Item -> AppHandler () -> AppHandler ()
authcheck user item hndlr =
  if siteUserAdmin user || siteUserId user == itemOwnerId item
     then hndlr
     else forbidden

-- | All API Handlers

apiNewItem :: Site -> SiteUser -> Int -> AppHandler ()
apiNewItem site user data_id = do
  d <- getDataById site data_id
  case d of
    Nothing -> passLog ["Data id not valid."]
    Just dat -> do
      r <- runForm "new-item" (fieldsForm (kvs (dataFields dat)))
      case r of
        (v, Nothing) ->
          renderWithSplices "api/data/new" (digestiveSplices v <> apiFieldsSplice dat)
        (_, Just flds) -> do
          newItem (Item (-1) (dataId dat) (dataSiteId dat) (siteUserId user) (fromList flds))
          modifyResponse (setResponseCode 201)
          render "api/data/new_success"

apiDeleteItem :: Site -> SiteUser -> Int -> AppHandler ()
apiDeleteItem site user item_id = do
  mit <- getItemById site item_id
  case mit of
    Nothing -> passLog ["Item id not valid"]
    Just item ->
      authcheck user item $
        (method GET $ render "api/data/delete")
        <|>
        (method POST $ do
          deleteItem item
          modifyResponse (setResponseCode 201)
          return ())

apiSetFieldItem :: Site -> SiteUser -> Int -> Text -> AppHandler ()
apiSetFieldItem site user item_id field = itemDataFieldSpecLookup site item_id field $ \item dat spec ->
  authcheck user item $ do
    r <- runForm "set-field" (fieldForm field spec (Just (itemFields item ! field)))
    case r of
      (v, Nothing) -> renderWithSplices "api/data/set" (digestiveSplices v
                                                  <> fieldsSplice (field, spec))
      (_, Just (_n, val)) -> do
          updateItem $ item { itemFields = insert field val (itemFields item)}
          modifyResponse (setResponseCode 201)
          return ()

apiListAddItem :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddItem user _site _item_id field item _dat spec = authcheck user item $ do
  r <- runForm "list-add" (fieldForm field spec Nothing)
  case r of
    (v, Nothing) -> renderWithSplices "api/data/set" (digestiveSplices v
                                              <> fieldsSplice (field, spec))
    (_, Just (_, val)) -> do
      let flds = itemFields item
      updateItem $ item { itemFields = insert field
                          (modifyListFieldElems (flds ! field) (val:)) flds}
      modifyResponse (setResponseCode 201)
      return ()

apiListDeleteItem :: SiteUser -> Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListDeleteItem user _site _item_id field idx item _dat _spec = authcheck user item $
  (method GET $ render "api/data/delete")
  <|>
  (method POST $ do
    let flds = itemFields item
    updateItem $ item { itemFields = insert field
                          (modifyListFieldElems (flds ! field) (removeAt idx)) flds}
    modifyResponse (setResponseCode 201)
    return ())
  where removeAt n lst = take n lst ++ drop (n + 1) lst

apiDeleteDataField :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiDeleteDataField user site item_id field item _dat spec' = authcheck user item $
  case spec' of
   DataFieldSpec nm ->
     (method GET $ render "api/data/delete")
     <|>
     (method POST $ do
       updateItem $ item { itemFields = insert field (DataFieldData Nothing) (itemFields item)}
       modifyResponse (setResponseCode 201)
       return ())

apiListSetItem :: SiteUser -> Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec ->  AppHandler ()
apiListSetItem user site item_id field idx item _dat spec = authcheck user item $ do
  let flds = itemFields item
  r <- runForm "list-set" (fieldForm field spec
                           (Just ((getListFieldElems $ flds ! field) !! idx)))
  case r of
    (v, Nothing) -> renderWithSplices "api/data/set" (digestiveSplices v
                                                 <> fieldsSplice (field, spec))
    (_, Just (_, val)) -> do
      updateItem $ item { itemFields = insert field
                          (modifyListFieldElems (flds ! field) (updateAt idx val)) flds}
      modifyResponse (setResponseCode 201)
      return ()


apiExistingDataHandler :: SiteUser -> Site  -> Text -> Item -> FieldSpec -> (Int -> FieldData -> FieldData) -> AppHandler ()
apiExistingDataHandler user site field item spec' field_update = authcheck user item $
  case spec' of
    DataFieldSpec nm -> do
      mdat <- getDataByName site nm
      case mdat of
        Nothing -> error $ "Bad data name: " ++ (T.unpack nm)
        Just dat -> do
          items <- if (siteUserAdmin user) then getItems dat else getUserItems dat user
          r <- runForm "field-data-existing" (fieldDataExistingForm items)
          case r of
            (v, Nothing) -> renderWithSplices "api/data/field/existing" (digestiveSplices v)
            (_, Just id') -> do
              let flds = itemFields item
              updateItem $ item { itemFields = insert field (field_update id' (flds ! field)) flds}
              modifyResponse (setResponseCode 201)
              return ()

apiSetDataFieldExisting :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetDataFieldExisting user site _item_id field item _dat spec' =
  apiExistingDataHandler user site field item  spec' $ \id' _ -> (DataFieldData (Just id'))

apiListAddDataExisting :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddDataExisting user site _item_id field item _dat spec' =
  apiExistingDataHandler user site field item spec' $ \id' fld -> modifyListFieldElems fld ((DataFieldData (Just id')):)

apiListSetDataExisting :: SiteUser -> Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListSetDataExisting user site item_id field idx item _dat spec' =
  apiExistingDataHandler user site field item spec' $ \id' fld ->
    modifyListFieldElems fld (updateAt idx (DataFieldData (Just id')))

apiNewDataHandler :: SiteUser -> Site -> Text -> Item -> FieldSpec -> (Int -> FieldData -> FieldData) -> AppHandler ()
apiNewDataHandler user site field item spec' field_update = authcheck user item $
  case spec' of
    DataFieldSpec nm -> do
      mdat <- getDataByName site nm
      case mdat of
        Nothing -> error $ "Bad data name: " ++ (T.unpack nm)
        Just dat -> do
         r <- runForm "field-data-new" (fieldsForm (kvs (dataFields dat)))
         case r of
           (v, Nothing) -> renderWithSplices "api/data/new" (digestiveSplices v <> apiFieldsSplice dat)
           (_, Just flds) -> do
             mid <- newItem (Item (-1) (dataId dat) (dataSiteId dat) 1 (fromList flds))
             case mid of
               Nothing -> error "Could not create new item"
               Just id' -> do
                 let flds = itemFields item
                 updateItem $ item { itemFields = insert field (field_update id' (flds ! field)) flds}
                 modifyResponse (setResponseCode 201)
                 return ()

apiSetDataFieldNew :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetDataFieldNew user site item_id field item _dat spec' =
  apiNewDataHandler user site field item spec' $ \id' _fld -> (DataFieldData (Just id'))


apiListAddDataNew :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddDataNew user site item_id field item _dat spec' =
  apiNewDataHandler user site field item spec' $ \id' fld -> modifyListFieldElems fld (DataFieldData (Just id'):)

apiListSetDataNew :: SiteUser -> Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListSetDataNew user site item_id field idx item _dat spec' =
  apiNewDataHandler user site field item spec' $ \id' fld ->
    modifyListFieldElems fld (updateAt idx (DataFieldData (Just id')))

apiSetImageField :: SiteUser -> Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetImageField user site item_id field item _dat spec' = authcheck user item $
  case spec' of
    ImageFieldSpec -> do
      r <- runFormWith (defaultSnapFormConfig { uploadPolicy = setMaximumFormInputSize (2^24) defaultUploadPolicy}) "image-form" imageForm
      case r of
        (v, Nothing) -> renderWithSplices "api/data/image" (digestiveSplices v)
        (_, Just path) -> do
          im <- newImage (siteId site)
          case im of
            Nothing -> error "Could not create image."
            Just image -> do
              repo <- getImageRepository
              let lf = loadFileSmart path
              case lf of
                Nothing -> error "Cannot support non-jpg or png"
                Just loadFile -> do
                  file <- liftIO (loadFile path)
                  (width, height) <- liftIO (imageSize file)
                  makeSizes file image width height repo standardSizes
                  updateImage image { imageFormats = standardSizes
                                    , imageExtension = "png"
                                    , imageOrigWidth = width
                                    , imageOrigHeight = height }
                  let flds = itemFields item
                  updateItem $ item { itemFields = insert field (ImageFieldData (imageId image)) flds}
                  modifyResponse (setResponseCode 201)
                  return ()
  where loadFileSmart path = if isSuffixOf ".jpg" path
                                then Just loadJpegFile
                                else if isSuffixOf ".png" path
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
