module Helpers.Misc where

import Data.Map (Map, assocs)
import Data.List (sort)
import Data.Maybe (listToMaybe)


readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads


kvs :: (Ord k, Ord v) => Map k v -> [(k, v)]
kvs = sort . assocs
