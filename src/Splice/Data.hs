{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Data where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift)
import Snap.Snaplet.Heist
import Heist
import Heist.Interpreted
import Application
import State.Data
import Helpers.Text

manageDataSplice :: [Data] -> Splice AppHandler
manageDataSplice = mapSplices (runChildrenWith . manageDatumSplices)

manageDatumSplices :: Data -> Splices (Splice AppHandler)
manageDatumSplices d = do
  "id" ## textSplice (tshow (dataId d))
  "name" ## textSplice (dataName d)
  "fields" ## textSplice (T.decodeUtf8 $ toStrict $ encode (dataFields d))

dataSplices :: Data -> Splices (Splice AppHandler)
dataSplices d = (T.append (dataName d) "-all")
                ## (renderAllItems d)

renderAllItems :: Data -> Splice AppHandler
renderAllItems d = do
  items <- lift $ getItems d
  mapSplices (runChildrenWith . (itemSplices d)) items

itemSplices :: Data -> Item -> Splices (Splice AppHandler)
itemSplices d i = foldr (<>) mempty $
  map (\(name, _spec) ->
          name ## fldSplice (M.lookup name (itemFields i)))
      (M.assocs $ dataFields d)

fldSplice :: Maybe FieldData -> Splice AppHandler
fldSplice Nothing = textSplice ""
fldSplice (Just (FieldDataString s)) = textSplice s
fldSplice (Just (FieldDataNumber n)) = textSplice (tshow n)
