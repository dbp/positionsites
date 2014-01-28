{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module State.Blob where

import Data.Typeable
import Control.Applicative
import Snap.Snaplet
import Control.Monad.Trans (liftIO)
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Blaze.ByteString.Builder (fromByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Configurator as C
import Data.List (sortBy)
import Data.Ord (comparing)

import Application
import Helpers.State
import Helpers.Misc
import Helpers.Text
import State.Site

data Blob = Blob { blobId :: Int
                 , blobSiteId :: Int
                 , blobName :: Text
                 , blobContent :: Text
                 , blobType :: BlobType
                 , blobAdmin :: Bool
                 }
instance FromRow Blob where
  fromRow = Blob <$> field <*> field <*> field <*> field <*> field  <*> field

data BlobType = BlobPlain | BlobMarkdown | BlobHTML deriving (Eq, Show, Typeable)

instance FromField BlobType where
  fromField _ (Just "plain") = pure BlobPlain
  fromField _ (Just "markdown") = pure BlobMarkdown
  fromField _ (Just "html") = pure BlobHTML
  fromField f _  = returnError ConversionFailed f "data doesn't look like BlobType"

instance ToField BlobType where
  toField BlobPlain = Escape "plain"
  toField BlobMarkdown = Escape "markdown"
  toField BlobHTML = Escape "html"

getSiteBlobs :: Site -> AppHandler [Blob]
getSiteBlobs site = query "select id, site_id, name, content, type, admin_only from blobs where site_id = ?" (Only (siteId site))

getBlobById :: Int -> Site -> AppHandler (Maybe Blob)
getBlobById id' site = singleQuery "select id, site_id, name, content, type, admin_only from blobs where id = ? and site_id = ?" (id', siteId site)

getBlobByName :: Site -> Text -> AppHandler (Maybe Blob)
getBlobByName site name = singleQuery "select id, site_id, name, content, type, admin_only from blobs where name = ? and site_id = ?" (name, siteId site)

newBlob :: Blob -> AppHandler (Maybe Int)
newBlob (Blob _ si n c t a) = idQuery "insert into blobs (site_id, name, content, type, admin_only) values (?,?,?,?,?) returning id" (si, n, c, t, a)

updateBlob :: Blob -> AppHandler ()
updateBlob (Blob i si n c t a) = void $ execute "update blobs set site_id = ?, name = ?, content = ?, type = ?, admin_only = ? where id = ?" (si, n, c, t, a, i)
