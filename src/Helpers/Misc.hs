{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Helpers.Misc where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Map (Map, assocs)
import Data.List (sort)
import Data.Maybe (listToMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T (concat, unpack)
import Application (AppHandler)
import Snap.Core (pass, modifyResponse, setResponseCode)
import "mtl" Control.Monad.Trans (liftIO)
import Snap.Snaplet.Auth (AuthUser(..), UserId(..))


fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b
trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

breadSafe :: Read a => ByteString -> Maybe a
breadSafe = readSafe . unpack

treadSafe :: Read a => Text -> Maybe a
treadSafe = readSafe . T.unpack

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


fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight: expected Right"
fromRight (Right x) = x


forbidden :: AppHandler ()
forbidden = do modifyResponse (setResponseCode 401)
               return ()

swapList :: Int -> Int -> [a] -> [a]
swapList ia ib lst = swap' ia ib lst 0 lst
 where
  swap' ia ib orig c [] = []
  swap' ia ib orig c (x:xs) = if c == ia
                                 then (orig !! ib) : (swap' ia ib orig (c+1) xs)
                                 else if c == ib
                                         then (orig !! ia) : (swap' ia ib orig (c+1) xs)
                                         else x : (swap' ia ib orig (c+1) xs)
