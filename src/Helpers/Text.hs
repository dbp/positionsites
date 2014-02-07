{-# LANGUAGE OverloadedStrings #-}

module Helpers.Text where

import qualified Data.Text as T
import           Data.Text (Text)
import           Text.XmlHtml
import           Data.List

tshow :: Show a => a -> Text
tshow = T.pack . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

newlineReplace :: Text -> [Node]
newlineReplace t = intersperse (Element "br" [] []) (map TextNode $ T.splitOn "\n" t)
