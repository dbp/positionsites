{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Data.ByteString (ByteString)
import Snap.Snaplet
import Application

routes :: [(ByteString, Handler App App ())]
routes = []
