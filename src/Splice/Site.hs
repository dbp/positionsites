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
  "site_id" ## textSplice (tshow (siteId s))
  "domains" ## domainsSplice s
  "base" ## textSplice (siteBase s)

domainsSplice :: Site -> Splice AppHandler
domainsSplice s =
  do urls <- lift $ getSiteUrls s
     mapSplices (runChildrenWith . (\u -> do "url_id" ## textSplice (tshow (fst u))
                                             "url" ## textSplice (snd u))) urls
