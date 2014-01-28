{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Blob where

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
import State.Blob
import Helpers.Text
import Helpers.Misc


manageBlobsSplice :: [Blob] -> Splice AppHandler
manageBlobsSplice = mapSplices (runChildrenWith . manageBlobSplices)

manageBlobSplices :: Blob -> Splices (Splice AppHandler)
manageBlobSplices b = do
  "id" ## textSplice (tshow (blobId b))
  "site_id" ## textSplice (tshow (blobSiteId b))
  "name" ## textSplice (blobName b)
  "is-plain" ## ifISplice (blobType b == BlobPlain)
  "is-markdown" ## ifISplice (blobType b == BlobMarkdown)
  "is-html" ## ifISplice (blobType b == BlobHTML)
  "is-admin-only" ## ifISplice (blobAdmin b)
  "not-admin-only" ## ifISplice (not (blobAdmin b))
  "content" ## textSplice (blobContent b)
