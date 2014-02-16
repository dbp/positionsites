{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.User where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import qualified Data.Map as M
import Snap.Snaplet.Heist
import Heist
import Heist.Splices
import Heist.Interpreted
import Application
import State.User
import Helpers.Text
import Helpers.Misc


manageUsersSplice :: [(SiteUser, Text)] -> Splice AppHandler
manageUsersSplice = mapSplices (runChildrenWith . manageUserSplices)

manageUserSplices :: (SiteUser, Text) -> Splices (Splice AppHandler)
manageUserSplices (SiteUser id' _ adm, login) = do
  "id" ## textSplice (tshow id')
  "is-admin" ## ifISplice adm
  "not-admin" ## ifISplice (not adm)
  "login" ## textSplice login


ownerSplice :: Int -> Splice AppHandler
ownerSplice i = do mu <- lift $ getUserAndNameById i
                   case mu of
                     Nothing -> return []
                     Just r -> runChildrenWith (manageUserSplices r)
