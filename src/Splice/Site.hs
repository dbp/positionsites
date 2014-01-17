{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Site where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift)
import Snap.Snaplet.Heist
import Heist
import Heist.Interpreted
import Application
import State.Site
import Helpers.Text
import Helpers.Misc


sitesSplice :: [Site] -> Splice AppHandler
sitesSplice = mapSplices (runChildrenWith . siteSplices)

siteSplices :: Site -> Splices (Splice AppHandler)
siteSplices s = do
  "id" ## textSplice (tshow (siteId s))
  "domain" ## textSplice (siteUrl s)
