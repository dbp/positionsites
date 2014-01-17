{-# LANGUAGE OverloadedStrings  #-}

module Handler.Index where


import Heist
import Snap.Snaplet.Heist
import State.Site
import Splice.Site
import Application

indexHandler :: AppHandler ()
indexHandler = do
  sites <- getSites
  renderWithSplices "index" ("sites" ## sitesSplice sites)
