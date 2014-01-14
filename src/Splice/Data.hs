{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Data where

import qualified Data.Text as T
import Data.Monoid
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift)
import Snap.Snaplet.Heist
import Heist
import Heist.Interpreted
import Application
import State.Data
import Helpers.Text


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
