{-# LANGUAGE OverloadedStrings  #-}

module State.User where

import Control.Monad (void)
import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.ByteString (ByteString)
import Data.Text (Text)
import Application
import State.Site
import Helpers.State

data SiteUser = SiteUser { siteUserId :: Int
                         , siteUserAdmin :: Bool
                         }

instance FromRow SiteUser where
  fromRow = SiteUser <$> field <*> field

newUser :: SiteUser -> AppHandler ()
newUser (SiteUser uid adm) = void $ execute "insert into users (id, admin) values (?,?)" (uid, adm)

getUser :: Int -> AppHandler (Maybe SiteUser)
getUser id' = singleQuery "select id, admin from users where id = ?" (Only id')
