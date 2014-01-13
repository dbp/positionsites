{-# LANGUAGE OverloadedStrings  #-}

module Handler.Index where

import Snap.Snaplet.Heist
import Application

indexHandler :: AppHandler ()
indexHandler = render "index"
