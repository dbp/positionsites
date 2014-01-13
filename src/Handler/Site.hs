{-# LANGUAGE OverloadedStrings  #-}

module Handler.Site where

import Heist
import Heist.Interpreted (textSplice)
import Snap.Snaplet.Heist
import Application
import State.Site

newSiteHandler = undefined

siteHandler :: Site -> AppHandler ()
siteHandler site = renderWithSplices "site/index"
                   ("domain" ## textSplice (siteUrl site))
