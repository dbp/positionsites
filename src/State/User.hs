{-# LANGUAGE OverloadedStrings  #-}

module State.User where

import Control.Monad (void, forM)
import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe
import Application
import State.Site
import Helpers.State

data SiteUser = SiteUser { siteUserId :: Int
                         , siteUserSiteId :: Int
                         , siteUserAdmin :: Bool
                         } deriving (Eq, Show)

instance FromRow SiteUser where
  fromRow = SiteUser <$> field <*> field <*> field

newUser :: SiteUser -> AppHandler ()
newUser (SiteUser uid si adm) =
  void $ do execute "insert into users (id, admin) values (?,?)" (uid, adm)
            execute "insert into users_sites (user_id, site_id) values (?,?)" (uid, si)

newSiteUser :: SiteUser -> AppHandler ()
newSiteUser (SiteUser uid si _) = void $ execute "insert into users_sites (user_id, site_id) values (?,?)" (uid, si)

getUser :: Int -> AppHandler (Maybe SiteUser)
getUser id' = singleQuery "select id, site_id, admin from users join users_sites on id = user_id where id = ?" (Only id')

deleteSiteUser :: Site -> Int -> AppHandler ()
deleteSiteUser site i = void $ execute "delete from users_sites where user_id = ? and site_id = ?" (i, siteId site)

deleteUser :: Int -> AppHandler ()
deleteUser i = void $ execute "delete from snap_auth_user where uid = ?" (Only i)


instance FromRow Text where
  fromRow = field

getUserNameById :: Int -> AppHandler (Maybe Text)
getUserNameById id' = singleQuery "select login from snap_auth_user where uid = ?" (Only id')

getUserAndNameById :: Int -> AppHandler (Maybe (SiteUser, Text))
getUserAndNameById i = do r <- query "select U.id, S.site_id, U.admin, A.login from users as U join users_sites as S on U.id = S.user_id join snap_auth_user as A on A.uid = U.id where U.id = ?" (Only i)
                          fmap listToMaybe $ forM r $ \(s :. Only t) -> return (s, t)

getSiteUsers :: Site -> AppHandler [(SiteUser, Text)]
getSiteUsers site = do
  res <- query "select U.id, S.site_id, U.admin, A.login from users as U join users_sites as S on U.id = S.user_id join snap_auth_user as A on A.uid = U.id where S.site_id = ?" (Only (siteId site))
  forM res $ \(s :. Only t) -> return (s, t)
