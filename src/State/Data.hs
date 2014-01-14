{-# LANGUAGE OverloadedStrings, DeriveDataTypeable,
             FlexibleInstances, TemplateHaskell  #-}

module State.Data where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.TH
import Data.Typeable
import Control.Applicative
import Blaze.ByteString.Builder (fromByteString
                                ,fromLazyByteString)
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Database.PostgreSQL.Simple.ToField hiding (Field)
import Database.PostgreSQL.Simple.Ok
import Snap.Snaplet.PostgresqlSimple
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict)

import Application
import State.Site
import Helpers.State

data Data = Data { dataId :: Int
                 , dataSiteId :: Int
                 , dataName :: Text
                 , dataFields :: Map Text FieldSpec
                 }
            deriving (Show, Eq)

instance FromRow Data where
  fromRow = Data <$> field <*> field <*> field <*> field

data FieldSpec = StringFieldSpec | NumberFieldSpec
                 deriving (Show, Eq, Typeable)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''FieldSpec)

instance FromField (Map Text FieldSpec) where
  fromField _ Nothing = pure M.empty
  fromField f (Just fs) =
    case decode (fromStrict fs) of
      Just m -> pure m
      Nothing -> returnError ConversionFailed f ("Could not decode json: " ++ (B8.unpack fs))

fieldToBs :: FieldSpec -> ByteString
fieldToBs StringFieldSpec = "string"
fieldToBs NumberFieldSpec = "number"

instance ToField [FieldSpec] where
  toField flds = Plain (fromByteString $ B8.intercalate "," $ map fieldToBs flds)

data FieldData = FieldDataString Text | FieldDataNumber Int
                 deriving (Show, Eq, Typeable)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''FieldData)

data Item = Item { itemId :: Int
                 , itemDataId :: Int
                 , itemSiteId :: Int
                 , itemOwnerId :: Int
                 , itemFields :: Map Text FieldData
                 } deriving (Show, Eq)

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field
                 <*> field <*> field

instance FromField (Map Text FieldData) where
  fromField _ Nothing = pure M.empty
  fromField f (Just fs) =
    case decode (fromStrict fs) of
      Just m -> pure m
      Nothing -> returnError ConversionFailed f ("Could not decode json: " ++ (B8.unpack fs))

instance ToField (Map Text FieldData) where
  toField flds = Plain (fromLazyByteString $ encode flds)

-- Lookup functions
getData :: Site -> AppHandler [Data]
getData s = query "select id, site_id, name, fields from data where site_id = ?" (Only $ siteId s)

getItems :: Data -> AppHandler [Item]
getItems d = query "select id, data_id, site_id, owner_id, fields from items where data_id = ? and site_id = ?" (dataId d, dataSiteId d)

newData :: Data -> AppHandler (Maybe Int)
newData d = idQuery "insert into data (site_id, name, fields) values (?,?,?) returning id" (dataSiteId d, dataName d, encode (dataFields d))
