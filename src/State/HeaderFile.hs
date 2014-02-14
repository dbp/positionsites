{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module State.HeaderFile where

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

data HeaderFile = HeaderFile {  headerFileId :: Int
                              , headerFileSiteId :: Int
                              , headerFileType :: HeaderFileType
                              , headerFileName :: Text
                              , headerFileContent :: Text
                             }

instance FromRow HeaderFile where
  fromRow = HeaderFile <$> field <*> field <*> field <*> field <*> field

data HeaderFileType = HeaderCSS | HeaderJavascript deriving (Eq, Show, Typeable)

instance FromField HeaderFileType where
  fromField _ (Just "css") = pure HeaderCSS
  fromField _ (Just "js") = pure HeaderJavascript
  fromField f _  = returnError ConversionFailed f "data doesn't look like HeaderFileType"

instance ToField HeaderFileType where
  toField HeaderCSS = Escape "css"
  toField HeaderJavascript = Escape "js"

getSiteHeaders :: Site -> AppHandler [HeaderFile]
getSiteHeaders site = query "select id, site_id, type, name, content from header_files where site_id = ?" (Only (siteId site))

newHeader :: HeaderFile -> AppHandler (Maybe Int)
newHeader (HeaderFile _ si t n c) = idQuery "insert into header_files (site_id, type, name, content) values (?,?,?,?) returning id" (si, t, n, c)

updateHeader :: HeaderFile -> AppHandler ()
updateHeader (HeaderFile i si t n c) = void $ execute "update header_files set site_id = ?, type = ?, name = ?, content = ? where id = ?" (si, t, n, c, i)

deleteHeaderFile :: Int -> Site -> AppHandler ()
deleteHeaderFile id' site = void $ execute "delete from header_files where id = ? and site_id = ?" (id', siteId site)


getHeaderById :: Int -> Site -> AppHandler (Maybe HeaderFile)
getHeaderById id' site = singleQuery "select id, site_id, type, name, content from header_files where id = ? and site_id = ?" (id', siteId site)
