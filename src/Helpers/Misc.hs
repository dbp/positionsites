{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Helpers.Misc where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Map (Map, assocs)
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T (concat, unpack)
import Application (AppHandler)
import Snap.Core (pass)
import "mtl" Control.Monad.Trans (liftIO)


fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b
trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c


readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads


kvs :: (Ord k, Ord v) => Map k v -> [(k, v)]
kvs = sort . assocs


bsId :: Maybe ByteString -> Maybe Int
bsId Nothing = Nothing
bsId (Just i) = readSafe $ unpack i

passLog :: [Text] -> AppHandler ()
passLog ts = do
  liftIO $ putStrLn $ T.unpack $ T.concat ("passing: ":ts)
  pass

updateAt :: Int -> a -> [a] -> [a]
updateAt n val lst = take n lst ++ [val] ++ drop (n + 1) lst
