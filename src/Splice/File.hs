{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.File where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift)
import Snap.Snaplet.Heist
import Heist
import Heist.Splices
import Heist.Interpreted
import Application
import State.File
import Helpers.Text
import Helpers.Misc


manageFilesSplice :: [File] -> Splice AppHandler
manageFilesSplice = mapSplices (runChildrenWith . manageFileSplices)

manageFileSplices :: File -> Splices (Splice AppHandler)
manageFileSplices s = do
  "id" ## textSplice (tshow (fileId s))
  "site_id" ## textSplice (tshow (fileSiteId s))
  "name" ## textSplice (fileName s)
  "path" ## textSplice (filePath s)
