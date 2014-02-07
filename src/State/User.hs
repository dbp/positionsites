{-# LANGUAGE OverloadedStrings  #-}

module State.User where

import Control.Monad (void, forM)
import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.ByteString (ByteString)
import Data.Text (Text)
import Application
import State.Site
import Helpers.State

data SiteUser = SiteUser { siteUserId :: Int
                         , siteUserAdmin :: Bool
                         } deriving (Eq, Show)

instance FromRow SiteUser where
  fromRow = SiteUser <$> field <*> field

newUser :: SiteUser -> AppHandler ()
newUser (SiteUser uid adm) = void $ execute "insert into users (id, admin) values (?,?)" (uid, adm)

getUser :: Int -> AppHandler (Maybe SiteUser)
getUser id' = singleQuery "select id, admin from users where id = ?" (Only id')

getSiteUsers :: Site -> AppHandler [(SiteUser, Text)]
getSiteUsers site = do
  res <- query "select U.id, U.admin, A.login from users as U join users_sites as S on U.id = S.user_id join snap_auth_user as A on A.uid = U.id where S.site_id = ?" (Only (siteId site))
  forM res $ \(s :. Only t) -> return (s, t)
