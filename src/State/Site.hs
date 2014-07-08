{-# LANGUAGE OverloadedStrings  #-}

module State.Site where

import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.Text (Text)

import Application
import Helpers.State

data Site = Site { siteId :: Int
                 , siteBase :: Text
                 , siteAnalyzeToken :: Maybe Text
                 }

instance FromRow Site where
  fromRow = Site <$> field <*> field <*> field

newSite :: Site -> AppHandler (Maybe Int)
newSite (Site _ base token) = idQuery "insert into sites (site_base, analyze_token) values (?, ?) returning id" (base, token)

getSiteByName :: Text -> AppHandler (Maybe Site)
getSiteByName name = singleQuery "select S.id, S.site_base, S.analyze_token from sites as S join site_urls as U on U.site_id = S.id where U.url = ?" (Only name)

getSiteById :: Int -> AppHandler (Maybe Site)
getSiteById id' = singleQuery "select id, site_base, analyze_token from sites where id = ?" (Only id')

getSites :: AppHandler [Site]
getSites = query_ "select id, site_base, analyze_token from sites"

getUserSites :: Int -> AppHandler [Site]
getUserSites i = query "select id, site_base, analyze_token from sites as S join users_sites as US on US.site_id = S.id where US.user_id = ?" (Only i)

updateSite :: Site -> AppHandler ()
updateSite (Site id' base token) = void $ execute "update sites set site_base = ?, analyze_token = ? where id = ?" (base, token, id')


getSiteUrls :: Site -> AppHandler [(Int, Text)]
getSiteUrls (Site id' _ _) = query "select id, url from site_urls where site_id = ? order by id desc" (Only id')

newDomain :: Site -> Text -> AppHandler ()
newDomain site url = void $ execute "insert into site_urls (site_id, url) values (?,?)" (siteId site, url)

deleteDomain :: Int -> Site -> AppHandler ()
deleteDomain id' site = void $ execute "delete from site_urls where site_id = ? and id = ?" (siteId site, id')
