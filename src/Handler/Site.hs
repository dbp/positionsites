{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Handler.Site where

import Data.Monoid
import Snap.Core
import Snap.Snaplet
import Heist
import Heist.Interpreted (textSplice, addTemplate
                         ,renderTemplate, bindSplices)
import Snap.Snaplet.Heist
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.Either
import Text.XmlHtml
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.Digestive
import Text.Digestive.Snap
import Text.Digestive.Heist

import Application
import State.Site
import State.Page
import State.Data
import Splice.Data
import Helpers.Forms
import Helpers.Misc

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
          renderWithSplices "site/index" ("domain" ## textSplice (siteUrl site))


-- What follows is routing the frontend of the site, ie when accessed from the
-- site's domain.

siteHandler :: Site -> AppHandler ()
siteHandler site = do
  pages <- getPages site
  routePages site pages

routePages :: Site -> [Page] -> AppHandler ()
routePages site pgs =
  route (map (\p -> (pageFlat p, renderPage site p))
             pgs)


renderPage :: Site -> Page -> AppHandler ()
renderPage s p = do
  ds <- getData s
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
