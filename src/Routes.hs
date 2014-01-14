{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Core

import Application
import Handler.Auth
import Handler.Site
import Handler.Index

import State.Site

routes :: [(ByteString, AppHandler ())]
routes = [("", domainHandler)]

domainHandler :: AppHandler ()
domainHandler = do
  name <- fmap rqServerName getRequest
  case name of
   "hosting" -> route managementRoutes
   _ -> do
     msite <- getSiteByName (decodeUtf8 name)
     case msite of
      Nothing -> pass
      Just site -> siteHandler site

managementRoutes :: [(ByteString, AppHandler ())]
managementRoutes = [("/login", loginHandler)
                   ,("/logout", logoutHandler)
                   ,("/reset", resetHandler)
                   ,("/forgot", forgotHandler)
                   ,("/signup", signupHandler)
                   ,("/", ifTop indexHandler)
                   ,("/new", newSiteHandler)
                   ,("/site/:id", manageSiteHandler)
                   ]
