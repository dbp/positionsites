{-# LANGUAGE OverloadedStrings  #-}

module State.Site where

import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.Text (Text)

import Application
import Helpers.State

data Site = Site { siteId :: Int
                 , siteUrl :: Text
                 , siteBase :: Text
                 }

instance FromRow Site where
  fromRow = Site <$> field <*> field <*> field

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ url base) = idQuery "insert into sites (url, site_base) values (?, ?) returning id" (url, base)

getSiteByName :: Text -> AppHandler (Maybe Site)
getSiteByName name = singleQuery "select id, url, site_base from sites where url = ?" (Only name)

getSiteById :: Int -> AppHandler (Maybe Site)
getSiteById id' = singleQuery "select id, url, site_base from sites where id = ?" (Only id')

getSites :: AppHandler [Site]
getSites = query_ "select id, url, site_base from sites"

updateSite :: Site -> AppHandler ()
updateSite (Site id' url base) = void $ execute "update sites set url = ?, site_base = ? where id = ?" (url, base, id')
