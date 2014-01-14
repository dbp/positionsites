module Helpers.Misc where

import Data.Maybe (listToMaybe)


readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads
