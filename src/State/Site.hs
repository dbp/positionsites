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
                 , siteAnalyzeToken :: Maybe Text
                 }

instance FromRow Site where
  fromRow = Site <$> field <*> field <*> field <*> field

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ url base token) = idQuery "insert into sites (url, site_base, analyze_token) values (?, ?, ?) returning id" (url, base, token)

getSiteByName :: Text -> AppHandler (Maybe Site)
getSiteByName name = singleQuery "select id, url, site_base, analyze_token from sites where url = ?" (Only name)

getSiteById :: Int -> AppHandler (Maybe Site)
getSiteById id' = singleQuery "select id, url, site_base, analyze_token from sites where id = ?" (Only id')

getSites :: AppHandler [Site]
getSites = query_ "select id, url, site_base, analyze_token from sites"

updateSite :: Site -> AppHandler ()
updateSite (Site id' url base token) = void $ execute "update sites set url = ?, site_base = ?, analyze_token = ? where id = ?" (url, base, token, id')
