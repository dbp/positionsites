{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T (readFile)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Snap.Snaplet
import Snap.Core
import Snap.Util.FileServe (serveDirectory)
import Web.Analyze.Client (wrap)
import Data.Configurator (require)
import Network.HTTP.Conduit (Manager)
import Text.Sundown.Html.Text
import System.Directory (doesFileExist)
import Text.XmlHtml
import Heist
import Snap.Snaplet.Heist

import Application
import Handler.Auth
import Handler.Site
import Handler.Index

import State.Site

routes :: Manager -> [(ByteString, AppHandler ())]
routes man = [("/static", serveDirectory "static")
             ,("", domainHandler man)]

domainHandler :: Manager -> AppHandler ()
domainHandler man = do
  name <- fmap rqServerName getRequest
  case name of
   "hosting" -> manRoute
   "positionsites.com" -> manRoute
   _ -> do
     msite <- getSiteByName (decodeUtf8 name)
     case msite of
      Nothing -> pass
      Just site -> siteHandler man site
  where manRoute = do conf <- getSnapletUserConfig
                      tok <- liftIO $ require conf "analyze-token"
                      wrap renderError man tok (route managementRoutes)

managementRoutes :: [(ByteString, AppHandler ())]
managementRoutes = [("/login", loginHandler)
                   ,("/logout", logoutHandler)
                   ,("/reset", resetHandler)
                   ,("/forgot", forgotHandler)
                   ,("/signup", signupHandler)
                   ,("/", ifTop indexHandler)
                   ,("/new", requireAdmin newSiteHandler)
                   ,("/site/:site_id", requireAdmin manageSiteHandler)
                   ,("/docs", handleDocs)
                   ]

handleDocs :: AppHandler ()
handleDocs = do pth <- fmap rqPathInfo getRequest
                let pth' = case pth of
                             "" -> "index"
                             _ -> pth
                let path = "markdown/" ++  (B8.unpack pth') ++ ".md"
                e <- liftIO $ doesFileExist path
                if e
                   then do f <- liftIO $ T.readFile path
                           let html = renderHtml allExtensions noHtmlModes False Nothing f
                           let ee = parseHTML path (encodeUtf8 html)
                           case ee of
                             Left err -> error ("Parsing error on markdown: " ++ err)
                             Right h -> renderWithSplices "base"
                                                          ("apply-content" ## return (docContent h))
                   else pass
