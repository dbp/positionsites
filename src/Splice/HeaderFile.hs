{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.HeaderFile where

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
import State.HeaderFile
import Helpers.Text
import Helpers.Misc


manageHeadersSplice :: [HeaderFile] -> Splice AppHandler
manageHeadersSplice = mapSplices (runChildrenWith . manageHeaderSplices)

manageHeaderSplices :: HeaderFile -> Splices (Splice AppHandler)
manageHeaderSplices s = do
  "id" ## textSplice (tshow (headerFileId s))
  "site_id" ## textSplice (tshow (headerFileSiteId s))
  "name" ## textSplice (headerFileName s)
  "is-css" ## ifISplice (headerFileType s == HeaderCSS)
  "is-js" ## ifISplice (headerFileType s == HeaderJavascript)
  "content" ## textSplice (headerFileContent s)
