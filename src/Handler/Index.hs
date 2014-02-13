{-# LANGUAGE OverloadedStrings  #-}

module Handler.Index where


import qualified Data.Text as T
import Snap.Snaplet
import Heist
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import State.Site
import Splice.Site
import Application

indexHandler :: AppHandler ()
indexHandler = do
  mu <- with auth currentUser
  sites <- case fmap (read . T.unpack . unUid) (mu >>= userId) of
             Nothing -> return []
             Just uid -> getUserSites uid
  renderWithSplices "index" ("sites" ## sitesSplice sites)
