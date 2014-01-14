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
import Application
import State.Site
import State.Page
import State.Data
import Splice.Data

newSiteHandler = undefined

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
