{-# LANGUAGE OverloadedStrings  #-}

module State.Page where

import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.ByteString (ByteString)
import Data.Text (Text)
import Application
import State.Site
import Helpers.State

data Page = Page { pageId :: Int
                 , pageSiteId :: Int
                 , pageFlat :: ByteString
                 , pageStructured :: Text
                 , pageBody :: Text
                 }

instance FromRow Page where
  fromRow = Page <$> field <*> field <*> field
                 <*> field <*> field

getSitePages :: Site -> AppHandler [Page]
getSitePages site = query "select id, site_id, flat, structured, body from pages where site_id = ?" (Only (siteId site))

newPage :: Page -> AppHandler (Maybe Int)
newPage (Page _ si fl st bd) = idQuery "insert into pages (site_id, flat, structured, body) values (?,?,?,?) returning id" (si, fl, st, bd)
