{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Page where

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
import State.Page
import Helpers.Text
import Helpers.Misc


managePagesSplice :: [Page] -> Splice AppHandler
managePagesSplice = mapSplices (runChildrenWith . managePageSplices)

managePageSplices :: Page -> Splices (Splice AppHandler)
managePageSplices p = do
  "id" ## textSplice (tshow (pageId p))
  "flat" ## textSplice (T.decodeUtf8 $ pageFlat p)
  "structured" ## textSplice (pageStructured p)
  "body-html" ## textSplice (pageBody p)
