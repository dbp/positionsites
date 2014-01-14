{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections  #-}

module Handler.Site where

import Control.Applicative
import Data.Map (Map, assocs, fromList)
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
import Text.XmlHtml
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
siteApiHandler site = route [("/new/:id", apiNewItem site)]


getDataFields :: [(Text, FieldSpec)] -> AppHandler (Either Text [(Text, FieldData)])
getDataFields [] = return $ Right []
getDataFields (x:xs) = do
  res <- getDataFields xs
  case res of
    Left err -> return $ Left err
    Right flds -> do
      p <- getParam (encodeUtf8 $ fst x)
      case p of
        Nothing -> return $ Left $ T.concat ["Param not found: ", fst x]
        Just param ->
          case snd x of
            StringFieldSpec -> return $ Right $ (fst x, StringFieldData $ decodeUtf8 param) : flds
            NumberFieldSpec ->
              case readSafe (T.unpack $ decodeUtf8 param) of
                Just n -> return $ Right $ (fst x, NumberFieldData n) : flds
                Nothing -> return $ Left $ T.concat ["Param '", fst x, "' should have been a number: "
                                                    , decodeUtf8 param]

apiNewItem :: Site -> AppHandler ()
apiNewItem site = do
  mid <- getParam "id"
  case bsId mid of
    Nothing -> pass
    Just data_id -> do
      d <- getDataById site data_id
      case d of
        Nothing -> pass
        Just dat -> do
          (method POST $ do
            res <- getDataFields (kvs $ dataFields dat)
            case res of
              Left err -> renderWithSplices "api/error" ("error" ## textSplice err)
              Right fields -> do
                newItem (Item (-1) (dataId dat) (dataSiteId dat) 1 (fromList $ fields))
                return ())
          <|>
          (method GET $ do
              let splices = apiDataSplices dat
              renderWithSplices "api/data/new" splices)

routePages :: Site -> [Page] -> AppHandler ()
routePages site pgs =
  route (map (\p -> (pageFlat p, renderPage site p))
             pgs)


renderPage :: Site -> Page -> AppHandler ()
renderPage s p = do
  ds <- getSiteData s
  let splices = foldr (<>) mempty $ map dataSplices ds
  modifyResponse (setContentType "text/html")
  case parseHTML "" (encodeUtf8 $ pageBody p) of
    Left err -> error (show err)
    Right html -> do
      st <- fmap (either (error.show) id) $
        liftIO $ runEitherT $ initHeist mempty
      let newst = addTemplate "page" (docContent html) Nothing st
      let newst' = bindSplices splices newst
      res <- renderTemplate newst' "page"
      case res of
        Nothing -> error "Could not render template"
        Just (builder, _) -> writeBuilder builder
