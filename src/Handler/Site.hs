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
import Heist.Interpreted (Splice, textSplice, addTemplate
                         ,renderTemplate, bindSplices, bindSplice
                         ,runChildrenWith, lookupSplice)
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
import Handler.API

sitePath :: Site -> ByteString
sitePath (Site id' _ _) = B.append "/site/" (B8.pack (show id'))


siteForm :: Maybe Site -> Form Text AppHandler (Text, Text)
siteForm old = (,) <$> "base" .: validateHtml (nonEmpty (text (fmap siteBase old)))
                   <*> "domain" .: nonEmpty (text (fmap siteUrl old))

newSiteHandler :: AppHandler ()
newSiteHandler = do
  r <- runForm "new-site" (siteForm Nothing)
  case r of
    (v, Nothing) -> renderWithSplices "site/new" (digestiveSplices v)
    (_, Just (url, base)) -> do
      mid <- newSite (Site (-1) url base)
      case mid of
        Nothing -> error "Site could not be created"
        Just site_id -> redirect (sitePath (Site site_id "" ""))

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
                ,("/edit", editSiteHandler site)
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

editSiteHandler :: Site -> AppHandler ()
editSiteHandler site = do
  r <- runForm "edit-base" (siteForm (Just site))
  case r of
    (v, Nothing) -> renderWithSplices "site/edit" (digestiveSplices v)
    (_, Just (base, domain)) -> do
      updateSite (site { siteBase = base, siteUrl = domain })
      redirect (sitePath site)

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
                   <*> "body" .: validateHtml (nonEmpty (text $ fmap trd3 p))

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


routePages :: Site -> [Page] -> AppHandler ()
routePages site pgs =
  route (map (\p -> (pageFlat p, renderPage site p))
             pgs)

rebindSplice :: Splice AppHandler
rebindSplice = do
  node <- getParamNode
  let attrs = do o <- getAttribute "old" node
                 n <- getAttribute "new" node
                 return (o, n)
  case attrs of
    Nothing -> return []
    Just (old, new) -> do
      st <- getHS
      let spl = lookupSplice old st
      case spl of
        Nothing -> return []
        Just splice -> do
           modifyHS $ bindSplice new splice
           return []

renderPage :: Site -> Page -> AppHandler ()
renderPage s p = do
  urlDataSplices <- fmap mconcat (mapM (loadData s) (zip (T.splitOn "/" (decodeUtf8 (pageFlat p))) (T.splitOn "/" (pageStructured p))))
  ds <- getSiteData s
  let splices = mappend (mconcat $ map (dataSplices s) ds) ("rebind" ## rebindSplice)
  modifyResponse (setContentType "text/html")
  case parseHTML "" (encodeUtf8 $ pageBody p) of
    Left err -> error (show err)
    Right html -> do
      st <- fmap (either (error.show) id) $
        liftIO $ runEitherT $ initHeist $ mempty { hcTemplateLocations =
                                                   [loadTemplates "snaplets/heist/templates/sites"]
                                                 }
      let newst = addTemplate "site_base" (docContent
                  (fromRight (parseHTML "" (encodeUtf8 $ siteBase s)))) Nothing st
      let newst' = addTemplate "page" [Element "apply" [("template", "site")] (docContent html)]
                   Nothing newst
      let newst'' = bindSplices (urlDataSplices <> splices <> defaultLoadTimeSplices) newst'

      res <- renderTemplate newst'' "page"
      case res of
        Nothing -> error "Could not render template"
        Just (builder, _) -> writeBuilder builder

loadData :: Site -> (Text, Text) -> AppHandler (Splices (Splice AppHandler))
loadData site (f, s) | T.isPrefixOf "id(" s && T.isSuffixOf ")" s && T.isPrefixOf ":" f = do
  mparam <- getParam (encodeUtf8 (T.drop 1 f))
  case bsId mparam of
    Nothing -> passLog' ["Param missing or not an integer: ", f]
    Just id' -> do
      let name = fromJust $ (T.stripSuffix ")") =<< (T.stripPrefix "id(" s)
      mdat <- getDataByName site name
      case mdat of
        Nothing -> error $ "Unknown data: " ++ (T.unpack name)
        Just dat -> do
          mitem <- getItemById site id'
          case mitem of
            Nothing -> passLog' ["Id for item does not correspond to an item: ", tshow id']
            Just item ->
              case itemDataId item == dataId dat of
                False -> passLog' ["Id specified does not correspond to the right data type: ", tshow id', " for data ", name]
                True ->
                  return $ T.append "this-" name ## runChildrenWith (itemSplices site dat item)
 where passLog' a = passLog a >> return mempty
loadData _ _ = return mempty
