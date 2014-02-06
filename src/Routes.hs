{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Core
import Snap.Util.FileServe (serveDirectory)
import Web.Analyze.Client (wrap)
import Data.Configurator (require)
import Network.HTTP.Conduit (Manager)

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
                   ,("/", requireAdmin $ ifTop indexHandler)
                   ,("/new", requireAdmin newSiteHandler)
                   ,("/site/:id", requireAdmin manageSiteHandler)
                   ]
