{-# LANGUAGE OverloadedStrings  #-}

module State.Page where

import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Data.ByteString (ByteString)
import Data.Text (Text)
import Application
import State.Site

data Page = Page { pageId :: Int
                 , pageSiteId :: Int
                 , pageFlat :: ByteString
                 , pageStructured :: Text
                 , pageBody :: Text
                 }

instance FromRow Page where
  fromRow = Page <$> field <*> field <*> field
                 <*> field <*> field

getPages :: Site -> AppHandler [Page]
getPages site = query "select id, site_id, flat, structured, body from pages where site_id = ?" (Only (siteId site))
