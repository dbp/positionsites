{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections  #-}

module Handler.API where

import Prelude hiding (lookup)
import Control.Applicative
import Data.Map (fromList, lookup, insert, (!))
import Data.Monoid
import Snap.Core
import Snap.Snaplet.Heist
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
import Splice.Data
import Splice.Page
import Helpers.Forms
import Helpers.Misc
import Helpers.Text

-- | Routes for the sites API

siteApiHandler :: Site -> AppHandler ()
siteApiHandler site = route [("/new/:id", apiId $ apiNewItem site)
                            ,("/delete/:id", apiId $ apiDeleteItem site)
                            ,("/set/:id/image/:field", apiIdFieldItem site apiSetImageField)
                            ,("/set/:id/:field", apiIdField $ apiSetFieldItem site)
                            ,("/list/:id/:field/add", apiList site apiListAddItem)
                            ,("/list/:id/:field/delete/:index", apiListIndex site apiListDeleteItem)
                           ,("/list/:id/:field/set/:index", apiListIndex site apiListSetItem)
                           ,("/delete/:id/data/:field", apiIdFieldItem site apiDeleteDataField)
                           ,("/set/:id/data/:field/existing", apiIdFieldItem site apiSetDataFieldExisting)
                           ,("/set/:id/data/:field/new", apiIdFieldItem site apiSetDataFieldNew)
                           ,("/list/:id/:field/add/data/existing", apiList site apiListAddDataExisting)
                           ,("/list/:id/:field/add/data/new", apiList site apiListAddDataNew)
                           ,("/list/:id/:field/set/:index/data/existing", apiListIndex site apiListSetDataExisting)
                           ,("/list/:id/:field/set/:index/data/new", apiListIndex site apiListSetDataNew)
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

-- | All API Handlers

apiNewItem :: Site -> Int -> AppHandler ()
apiNewItem site data_id = do
  d <- getDataById site data_id
  case d of
    Nothing -> passLog ["Data id not valid."]
    Just dat -> do
      r <- runForm "new-item" (fieldsForm (kvs (dataFields dat)))
      case r of
        (v, Nothing) ->
          renderWithSplices "api/data/new" (digestiveSplices v <> apiFieldsSplice dat)
        (_, Just flds) -> do
          newItem (Item (-1) (dataId dat) (dataSiteId dat) 1 (fromList flds))
          modifyResponse (setResponseCode 201)
          render "api/data/new_success"

apiDeleteItem :: Site -> Int -> AppHandler ()
apiDeleteItem site item_id =do
  mit <- getItemById site item_id
  case mit of
    Nothing -> passLog ["Item id not valid"]
    Just item ->
      (method GET $ render "api/data/delete")
      <|>
      (method POST $ do
        deleteItem item
        modifyResponse (setResponseCode 201)
        return ())

apiSetFieldItem :: Site -> Int -> Text -> AppHandler ()
apiSetFieldItem site item_id field = itemDataFieldSpecLookup site item_id field $ \item dat spec ->
  do r <- runForm "set-field" (fieldForm field spec (Just (itemFields item ! field)))
     case r of
       (v, Nothing) -> renderWithSplices "api/data/set" (digestiveSplices v
                                                        <> fieldsSplice (field, spec))
       (_, Just (_n, val)) -> do
           updateItem $ item { itemFields = insert field val (itemFields item)}
           modifyResponse (setResponseCode 201)
           return ()

apiListAddItem :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddItem _site _item_id field item _dat spec = do
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

apiListDeleteItem :: Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListDeleteItem _site _item_id field idx item _dat _spec =
  (method GET $ render "api/data/delete")
  <|>
  (method POST $ do
    let flds = itemFields item
    updateItem $ item { itemFields = insert field
                          (modifyListFieldElems (flds ! field) (removeAt idx)) flds}
    modifyResponse (setResponseCode 201)
    return ())
  where removeAt n lst = take n lst ++ drop (n + 1) lst

apiDeleteDataField :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiDeleteDataField site item_id field item _dat spec' =
  case spec' of
   DataFieldSpec nm ->
     (method GET $ render "api/data/delete")
     <|>
     (method POST $ do
       updateItem $ item { itemFields = insert field (DataFieldData Nothing) (itemFields item)}
       modifyResponse (setResponseCode 201)
       return ())

apiListSetItem :: Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec ->  AppHandler ()
apiListSetItem site item_id field idx item _dat spec = do
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


apiExistingDataHandler :: Site  -> Text -> Item -> FieldSpec -> (Int -> FieldData -> FieldData) -> AppHandler ()
apiExistingDataHandler site field item spec' field_update =
  case spec' of
    DataFieldSpec nm -> do
      mdat <- getDataByName site nm
      case mdat of
        Nothing -> error $ "Bad data name: " ++ (T.unpack nm)
        Just dat -> do
          items <- getItems dat
          r <- runForm "field-data-existing" (fieldDataExistingForm items)
          case r of
            (v, Nothing) -> renderWithSplices "api/data/field/existing" (digestiveSplices v)
            (_, Just id') -> do
              let flds = itemFields item
              updateItem $ item { itemFields = insert field (field_update id' (flds ! field)) flds}
              modifyResponse (setResponseCode 201)
              return ()

apiSetDataFieldExisting :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetDataFieldExisting site _item_id field item _dat spec' =
  apiExistingDataHandler site field item  spec' $ \id' _ -> (DataFieldData (Just id'))

apiListAddDataExisting :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddDataExisting site _item_id field item _dat spec' =
  apiExistingDataHandler site field item spec' $ \id' fld -> modifyListFieldElems fld ((DataFieldData (Just id')):)

apiListSetDataExisting :: Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListSetDataExisting site item_id field idx item _dat spec' =
  apiExistingDataHandler site field item spec' $ \id' fld ->
    modifyListFieldElems fld (updateAt idx (DataFieldData (Just id')))

apiNewDataHandler :: Site -> Text -> Item -> FieldSpec -> (Int -> FieldData -> FieldData) -> AppHandler ()
apiNewDataHandler site field item spec' field_update =
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

apiSetDataFieldNew :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetDataFieldNew site item_id field item _dat spec' =
  apiNewDataHandler site field item spec' $ \id' _fld -> (DataFieldData (Just id'))


apiListAddDataNew :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiListAddDataNew site item_id field item _dat spec' =
  apiNewDataHandler site field item spec' $ \id' fld -> modifyListFieldElems fld (DataFieldData (Just id'):)

apiListSetDataNew :: Site -> Int -> Text -> Int -> Item -> Data -> FieldSpec -> AppHandler ()
apiListSetDataNew site item_id field idx item _dat spec' =
  apiNewDataHandler site field item spec' $ \id' fld ->
    modifyListFieldElems fld (updateAt idx (DataFieldData (Just id')))

apiSetImageField :: Site -> Int -> Text -> Item -> Data -> FieldSpec -> AppHandler ()
apiSetImageField site item_id field item _dat spec' =
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
