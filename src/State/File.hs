{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module State.File where

import Data.Typeable
import Control.Applicative
import Snap.Snaplet
import Control.Monad (when)
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
import System.Directory (copyFile, removeFile, doesFileExist)

import Application
import Helpers.State
import Helpers.Misc
import Helpers.Text
import State.Site

data File = File { fileId :: Int
                 , fileSiteId :: Int
                 , fileName :: Text
                 , filePath :: Text
                 }

instance FromRow File where
  fromRow = File <$> field <*> field <*> field <*> field

getFileRepository :: AppHandler Text
getFileRepository = do
  config <- getSnapletUserConfig
  dir <- liftIO $ C.lookup config "fileRepo"
  case dir of
    Nothing -> error "Could not get fileRepo from config."
    Just path -> return path

storeFile :: Text -> Text -> Site -> AppHandler Text
storeFile key tmp site = do repo <- getFileRepository
                            let p = T.unpack tmp
                            let ext = if "." `T.isInfixOf` tmp
                                      then reverse $ takeWhile (\c -> c /= '.' && c /= '/') $ reverse
                                      else ""
                            let newpath = T.concat ["/", tshow (siteId site)
                                                   , "_", key, ".", T.pack ext]
                            liftIO $ copyFile p (T.unpack (T.append repo newpath))
                            return newpath

newFile :: File -> AppHandler (Maybe Int)
newFile (File _ si n p) = idQuery "insert into files (site_id, name, path) values (?,?,?) returning id" (si, n, p)

updateFile :: File -> AppHandler ()
updateFile (File i si n p) = void $ execute "update files set site_id = ?, name = ?, path = ? where id = ?" (si, n, p, i)

deleteFile :: Int -> Site -> AppHandler ()
deleteFile id' site =
  do mp <- singleQuery "delete from files where id = ? and site_id = ? returning path" (id', siteId site)
     case mp of
       Nothing -> return ()
       Just (Only p) -> do
         repo <- getFileRepository
         let path = T.unpack (T.append repo p)
         e <- liftIO $ doesFileExist path
         when e $
           liftIO $ removeFile path

getFileById :: Int -> Site -> AppHandler (Maybe File)
getFileById id' site = singleQuery "select id, site_id, name, path from files where id = ? and site_id = ?" (id', siteId site)

getFileByName :: Text -> Site -> AppHandler (Maybe File)
getFileByName nm site = singleQuery "select id, site_id, name, path from files where name = ? and site_id = ?" (nm, siteId site)

getSiteFiles :: Site -> AppHandler [File]
getSiteFiles s = query "select id, site_id, name, path from files where site_id = ?" (Only (siteId s))
