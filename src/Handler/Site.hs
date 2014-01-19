{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections  #-}

module Handler.Site where

import Prelude hiding (lookup)
import Control.Applicative
import Data.Map (Map, assocs, fromList, lookup, insert, (!))
import Data.Monoid
import Data.Maybe
import Snap.Core
import Snap.Snaplet
import Heist
import Heist.Interpreted (textSplice, addTemplate
                         ,renderTemplate, bindSplices)
import Snap.Snaplet.Heist
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

import Application
import State.Site
import State.Page
import State.Data
import Splice.Data
import Splice.Page
import Helpers.Forms
import Helpers.Misc
import Helpers.Text

sitePath :: Site -> ByteString
sitePath (Site id' _) = B.append "/site/" (B8.pack (show id'))

newSiteForm :: Form Text AppHandler Text
newSiteForm = "url" .: nonEmptyTextForm

newSiteHandler :: AppHandler ()
newSiteHandler = do
  r <- runForm "new-site" newSiteForm
  case r of
    (v, Nothing) -> renderWithSplices "site/new" (digestiveSplices v)
    (_, Just url) -> do
      mid <- newSite (Site (-1) url)
      case mid of
        Nothing -> error "Site could not be created"
        Just site_id -> redirect (sitePath (Site site_id ""))

manageSiteHandler :: AppHandler ()
manageSiteHandler = do
  mid <- getParam "id"
  case fmap B8.unpack mid >>= readSafe of
    Nothing -> pass
    Just id' -> do
      msite <- getSiteById id'
      case msite of
        Nothing -> pass
        Just site ->
          route [("", ifTop $ showSiteHandler site)
                ,("/data/new", newDataHandler site)
                ,("/page/new", newPageHandler site)
                ,("/page/edit/:id", editPageHandler site)]

showSiteHandler :: Site -> AppHandler ()
showSiteHandler site = do
  ds <- getSiteData site
  pgs <- getSitePages site
  renderWithSplices "site/index" $ do
    "site_id" ## textSplice (tshow (siteId site))
    "domain" ## textSplice (siteUrl site)
    "data" ## manageDataSplice ds
    "pages" ## managePagesSplice pgs

newDataForm :: Form Text AppHandler (Text, Map Text FieldSpec)
newDataForm = (,) <$> "name"   .: nonEmptyTextForm
                  <*> "fields" .: jsonMapForm

newDataHandler :: Site -> AppHandler ()
newDataHandler site = do
  r <- runForm "new-data" newDataForm
  case r of
    (v, Nothing) -> renderWithSplices "data/new" (digestiveSplices v)
    (_, Just (name, fields)) -> do
      newData (Data (-1) (siteId site) name fields)
      redirect (sitePath site)

pageForm :: Maybe (Text,Text,Text) -> Form Text AppHandler (Text, Text, Text)
pageForm p = (,,) <$> "flat" .: nonEmpty (text $ fmap fst3 p)
                   <*> "structured" .: nonEmpty (text $ fmap snd3 p)
                   <*> "body" .: nonEmpty (text $ fmap trd3 p)

newPageHandler :: Site -> AppHandler ()
newPageHandler site = do
  r <- runForm "new-page" $ pageForm Nothing
  case r of
    (v, Nothing) -> renderWithSplices "page/new" (digestiveSplices v)
    (_, Just (flat, structured, body)) -> do
      newPage (Page (-1) (siteId site) (encodeUtf8 flat) structured body)
      redirect (sitePath site)

editPageHandler :: Site -> AppHandler ()
editPageHandler site = do
  mid <- getParam "id"
  case bsId mid of
    Nothing -> pass
    Just id' -> do
      mp <- getPageById id' site
      case mp of
        Nothing -> pass
        Just page -> do
          r <- runForm "edit-page" $ pageForm $ Just ( decodeUtf8 $ pageFlat page
                                                     , pageStructured page
                                                     , pageBody page)
          case r of
            (v, Nothing) -> renderWithSplices "page/edit" (digestiveSplices v)
            (_, Just (flat, structured, body)) -> do
              updatePage (page { pageFlat = encodeUtf8 flat
                               , pageStructured = structured
                               , pageBody = body})
              redirect (sitePath site)

-- What follows is routing the frontend of the site, ie when accessed from the
-- site's domain.

siteHandler :: Site -> AppHandler ()
siteHandler site =
  route [("/api", siteApiHandler site)
        ,("", do pages <- getSitePages site
                 routePages site pages)]

siteApiHandler :: Site -> AppHandler ()
siteApiHandler site = route [("/new/:id", apiId $ apiNewItem site)
                            ,("/delete/:id", apiId $ apiDeleteItem site)
                            ,("/set/:id/:field", apiIdField $ apiSetFieldItem site)
                            ,("/list/:id/:field/add", apiIdField $ apiListAddItem site)
                            ,("/list/:id/:field/delete/:index",
                              apiIdFieldIndex $ apiListDeleteItem site)
                            ,("/list/:id/:field/set/:index",
                               apiIdFieldIndex $ apiListSetItem site)
                            ,("/delete/:id/data/:field", apiIdField $ apiDeleteField site)
                            ,("/set/:id/data/:field/existing", apiIdField $ apiSetDataFieldExisting site)
                            ,("/set/:id/data/:field/new", apiIdField $ apiSetDataFieldNew site)
                            ]

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

apiListAddItem :: Site -> Int -> Text -> AppHandler ()
apiListAddItem site item_id field = itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
  case spec' of
    ListFieldSpec spec ->
      do r <- runForm "list-add" (fieldForm field spec Nothing)
         case r of
           (v, Nothing) -> renderWithSplices "api/data/set" (digestiveSplices v
                                                            <> fieldsSplice (field, spec))
           (_, Just (_, val)) -> do
             let flds = itemFields item
             updateItem $ item { itemFields = insert field
                                 (modifyListFieldElems (flds ! field) (val:)) flds}
             modifyResponse (setResponseCode 201)
             return ()
    _ -> passLog ["Tried to add a list item to a non-list."]

apiListDeleteItem :: Site -> Int -> Text -> Int -> AppHandler ()
apiListDeleteItem site item_id field idx =
  itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
    case spec' of
      ListFieldSpec _spec ->
        (method GET $ render "api/data/delete")
        <|>
        (method POST $ do
          let flds = itemFields item
          updateItem $ item { itemFields = insert field
                                (modifyListFieldElems (flds ! field) (removeAt idx)) flds}
          modifyResponse (setResponseCode 201)
          return ())
      _ -> passLog ["Tried to add a list item to a non-list."]
  where removeAt n lst = take n lst ++ drop (n + 1) lst

apiListSetItem :: Site -> Int -> Text -> Int ->  AppHandler ()
apiListSetItem site item_id field idx =
  itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
    case spec' of
      ListFieldSpec spec ->
        do let flds = itemFields item
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
  where updateAt n val lst = take n lst ++ [val] ++ drop (n + 1) lst


apiSetDataFieldExisting :: Site -> Int -> Text -> AppHandler ()
apiSetDataFieldExisting site item_id field =
  itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
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
               updateItem $ item { itemFields = insert field (DataFieldData (Just id')) (itemFields item)}
               modifyResponse (setResponseCode 201)
               return ()

apiSetDataFieldNew :: Site -> Int -> Text -> AppHandler ()
apiSetDataFieldNew site item_id field =
  itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
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
                  updateItem $ item { itemFields = insert field (DataFieldData (Just id')) (itemFields item)}
                  modifyResponse (setResponseCode 201)
                  return ()


apiDeleteField :: Site -> Int -> Text -> AppHandler ()
apiDeleteField site item_id field =
  itemDataFieldSpecLookup site item_id field $ \item _dat spec' ->
    case spec' of
     DataFieldSpec nm ->
       (method GET $ render "api/data/delete")
       <|>
       (method POST $ do
         updateItem $ item { itemFields = insert field (DataFieldData Nothing) (itemFields item)}
         modifyResponse (setResponseCode 201)
         return ())


routePages :: Site -> [Page] -> AppHandler ()
routePages site pgs =
  route (map (\p -> (pageFlat p, renderPage site p))
             pgs)


renderPage :: Site -> Page -> AppHandler ()
renderPage s p = do
  ds <- getSiteData s
  let splices = mconcat $ map (dataSplices s) ds
  modifyResponse (setContentType "text/html")
  case parseHTML "" (encodeUtf8 $ pageBody p) of
    Left err -> error (show err)
    Right html -> do
      st <- fmap (either (error.show) id) $
        liftIO $ runEitherT $ initHeist $ mempty { hcTemplateLocations =
                                                   [loadTemplates "snaplets/heist/templates/sites"]
                                                 }
      let newst = addTemplate "page" [Element "apply" [("template", "site")] (docContent html)]
                  Nothing st
      let newst' = bindSplices (splices <> defaultLoadTimeSplices) newst
      res <- renderTemplate newst' "page"
      case res of
        Nothing -> error "Could not render template"
        Just (builder, _) -> writeBuilder builder
