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

getSiteByName :: Text -> AppHandler (Maybe Site)
getSiteByName name = singleQuery "select id, url from sites where url = ?" (Only name)
