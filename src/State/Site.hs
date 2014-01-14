{-# LANGUAGE OverloadedStrings  #-}

module State.Site where

import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.Text (Text)

import Application
import Helpers.State

data Site = Site { siteId :: Int
                 , siteUrl :: Text }

instance FromRow Site where
  fromRow = Site <$> field <*> field

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ url) = idQuery "insert into sites (url) values (?) returning id" (Only url)

getSiteByName :: Text -> AppHandler (Maybe Site)
getSiteByName name = singleQuery "select id, url from sites where url = ?" (Only name)

getSiteById :: Int -> AppHandler (Maybe Site)
getSiteById id' = singleQuery "select id, url from sites where id = ?" (Only id')
