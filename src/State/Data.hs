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
import Control.Monad (mzero)
import Control.Monad.Trans (liftIO)
import Blaze.ByteString.Builder (fromByteString
                                ,fromLazyByteString)
import Database.PostgreSQL.Simple.FromField hiding (Field, Array)
import Database.PostgreSQL.Simple.ToField hiding (Field)
import Database.PostgreSQL.Simple.Ok
import Snap.Snaplet.PostgresqlSimple
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Vector as V

import Application
import State.Site
import Helpers.State
import Helpers.Misc
import Helpers.Text

data Data = Data { dataId :: Int
                 , dataSiteId :: Int
                 , dataName :: Text
                 , dataFields :: Map Text FieldSpec
                 }
            deriving (Show, Eq)

instance FromRow Data where
  fromRow = Data <$> field <*> field <*> field <*> field

data FieldSpec = StringFieldSpec | NumberFieldSpec | ListFieldSpec FieldSpec
                 deriving (Show, Eq, Typeable, Ord)

instance FromJSON FieldSpec where
     parseJSON (String "number") = return NumberFieldSpec
     parseJSON (String "string") = return StringFieldSpec
     parseJSON (Array arr)       = if V.length arr == 1
                                      then ListFieldSpec <$> (parseJSON (arr V.! 0))
                                      else mzero
     parseJSON _                 = mzero


instance ToJSON FieldSpec where
     toJSON NumberFieldSpec = String "number"
     toJSON StringFieldSpec = String "string"
     toJSON (ListFieldSpec sp) = Array (V.fromList [toJSON sp])

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


parseSpec :: FieldSpec -> Text -> Maybe FieldData
parseSpec StringFieldSpec s = Just $ StringFieldData s
parseSpec NumberFieldSpec s = fmap NumberFieldData (readSafe (unpack s))

data FieldData = StringFieldData Text | NumberFieldData Int | ListFieldData [FieldData]
                 deriving (Show, Eq, Typeable, Ord)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''FieldData)

renderFieldData :: FieldData -> Text
renderFieldData (StringFieldData s) = s
renderFieldData (NumberFieldData n) = tshow n
renderFieldData (ListFieldData elems) = T.concat $ ["["] ++ [T.intercalate ", " (map renderFieldData elems)] ++ ["]"]

modifyListFieldElems :: FieldData -> ([FieldData] -> [FieldData]) -> FieldData
modifyListFieldElems (ListFieldData elems) f = ListFieldData (f elems)
modifyListFieldElems x _ = error $ "Expected ListFieldData, got " ++ (show x)

isListFieldSpec :: FieldSpec -> Bool
isListFieldSpec (ListFieldSpec _) = True
isListFieldSpec _ = False

getListFieldElems :: FieldData -> [FieldData]
getListFieldElems (ListFieldData elems) = elems
getListFieldElems x = error $ "Expected ListFieldData, got " ++ (show x)

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
      Nothing -> returnError ConversionFailed f ("Could not decode json: " ++ B8.unpack fs)

instance ToField (Map Text FieldData) where
  toField flds = Plain (fromLazyByteString $ encode flds)

-- Lookup functions
getSiteData :: Site -> AppHandler [Data]
getSiteData s = query "select id, site_id, name, fields from data where site_id = ?" (Only $ siteId s)

getDataById :: Site -> Int -> AppHandler (Maybe Data)
getDataById s i = singleQuery "select id, site_id, name, fields from data where site_id = ? and id = ?" (siteId s, i)

getItems :: Data -> AppHandler [Item]
getItems d = query "select id, data_id, site_id, owner_id, fields from items where data_id = ? and site_id = ? order by id desc" (dataId d, dataSiteId d)

newData :: Data -> AppHandler (Maybe Int)
newData d = idQuery "insert into data (site_id, name, fields) values (?,?,?) returning id" (dataSiteId d, dataName d, encode (dataFields d))

getItemById :: Site -> Int -> AppHandler (Maybe Item)
getItemById site i = singleQuery "select id, data_id, site_id, owner_id, fields from items where id = ? and site_id = ?" (i, siteId site)

newItem :: Item -> AppHandler (Maybe Int)
newItem i = idQuery "insert into items (data_id, site_id, owner_id, fields) values (?,?,?,?) returning id" (itemDataId i, itemSiteId i, itemOwnerId i, encode (itemFields i))

deleteItem :: Item -> AppHandler ()
deleteItem item = void (execute "delete from items where id = ? and site_id = ?" (itemId item, itemSiteId item))

updateItem :: Item -> AppHandler ()
updateItem (Item i di si oi fs) = void (execute "update items set data_id = ?, site_id = ?, owner_id = ?, fields = ? where id = ?" (di, si, oi, encode fs, i))
