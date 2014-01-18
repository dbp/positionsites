{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Data where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift)
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X
import Heist
import Heist.Interpreted
import Application
import State.Data
import Helpers.Text
import Helpers.Misc

manageDataSplice :: [Data] -> Splice AppHandler
manageDataSplice = mapSplices (runChildrenWith . manageDatumSplices)

manageDatumSplices :: Data -> Splices (Splice AppHandler)
manageDatumSplices d = do
  "id" ## textSplice (tshow (dataId d))
  "name" ## textSplice (dataName d)
  "fields" ## textSplice (T.decodeUtf8 $ toStrict $ encode (dataFields d))
  "item-count" ## itemCountSplice d

itemCountSplice :: Data -> Splice AppHandler
itemCountSplice d = do
  count <- lift (itemCount d)
  textSplice (tshow count)

apiFieldsSplice :: Data -> Splices (Splice AppHandler)
apiFieldsSplice d = "fields" ## mapSplices (runChildrenWith . fieldsSplice) (kvs $ dataFields d)

fieldsSplice :: (Text, FieldSpec) -> Splices (Splice AppHandler)
fieldsSplice (n, StringFieldSpec) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "text"
fieldsSplice (n, NumberFieldSpec) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "text"
fieldsSplice (n, ListFieldSpec et) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "hidden"

apiDataFieldSplice :: FieldData -> Text -> Splices (Splice AppHandler)
apiDataFieldSplice (StringFieldData s) name = do
  "field-name" ## textSplice name
  "field-input" ## inputTextSplice "value" (Just s)
apiDataFieldSplice (NumberFieldData n) name = do
  "field-name" ## textSplice name
  "field-input" ## inputTextSplice "value" (Just (tshow n))

inputTextSplice :: Text -> Maybe Text -> Splice AppHandler
inputTextSplice n mt = return [X.Element "input" [("type", "text"), ("name", n), ("value", fromMaybe "" mt)] []]

inputNullSplice :: Text -> Splice AppHandler
inputNullSplice n = return [X.Element "input" [("type", "text"), ("disabled", "1"), ("value", "[set later]")] [], X.Element "input" [("type", "hidden"), ("name", n), ("value", "[]")] []]


dataSplices :: Data -> Splices (Splice AppHandler)
dataSplices d = do
  T.append "all-" (dataName d) ## renderAllItems d
  T.append "new-" (dataName d) ## newItemSplice d

renderAllItems :: Data -> Splice AppHandler
renderAllItems d = do
  items <- lift $ getItems d
  mapSplices (runChildrenWith . itemSplices d) items

itemSplices :: Data -> Item -> Splices (Splice AppHandler)
itemSplices d i = (T.concat ["delete-", dataName d] ## deleteSplice d i)
                  <>
    (mconcat $
     map (\(name, spec) -> do
             T.concat [dataName d, "-", name] ## fldSplice i name (M.lookup name (itemFields i))
             if isListFieldSpec spec
                then T.concat ["list-add-", dataName d, "-", name] ## addListFieldSplice i name
                else T.concat ["set-", dataName d, "-", name] ## setFieldSplice i name)
        (M.assocs $ dataFields d))

fldSplice :: Item -> Text -> Maybe FieldData -> Splice AppHandler
fldSplice _ _ Nothing = textSplice ""
fldSplice _ _ (Just (StringFieldData s)) = textSplice s
fldSplice _ _ (Just (NumberFieldData n)) = textSplice (tshow n)
fldSplice i n (Just (ListFieldData ls)) =
  mapSplices (runChildrenWith .
              (\(Just (idx, f)) -> do
                "delete-element" ## deleteListFieldSplice i n idx
                "set-element" ## setListFieldSplice i n idx
                "element" ## fldSplice i n (Just f)) .
              Just) (zip [0..] ls)

newItemSplice :: Data -> Splice AppHandler
newItemSplice d = do
  n <- getParamNode
  return [X.Element "a" [("href", T.concat ["/api/new/", tshow (dataId d)])
                             ,("data-box", "1")
                             ,("data-refresh", "page")]
                             (X.childNodes n)]

deleteSplice :: Data -> Item -> Splice AppHandler
deleteSplice d i = do
  n <- getParamNode
  return [X.Element "a" [("href", T.concat ["/api/delete/", tshow (itemId i)])
                         ,("data-box", "1")
                         ,("data-refresh", "page")]
                         (X.childNodes n)]

setFieldSplice :: Item -> Text -> Splice AppHandler
setFieldSplice i nm = do
  n <- getParamNode
  return [X.Element "a" [("href", T.concat ["/api/set/", tshow (itemId i), "/", nm])
                         ,("data-box", "1")
                         ,("data-refresh", "page")]
                         (X.childNodes n)]

addListFieldSplice :: Item -> Text -> Splice AppHandler
addListFieldSplice i nm = do
  n <- getParamNode
  return [X.Element "a" [("href", T.concat ["/api/list/", tshow (itemId i), "/", nm, "/add"])
                         ,("data-box", "1")
                         ,("data-refresh", "page")]
                         (X.childNodes n)]

deleteListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
deleteListFieldSplice i nm idx = do
  n <- getParamNode
  return [X.Element "a" [("href"
                        , T.concat ["/api/list/"
                                   , tshow (itemId i)
                                   , "/", nm, "/delete/"
                                   , tshow idx])
                         ,("data-box", "1")
                         ,("data-refresh", "page")]
                         (X.childNodes n)]

setListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
setListFieldSplice i nm idx = do
  n <- getParamNode
  return [X.Element "a" [("href"
                        , T.concat ["/api/list/"
                                   , tshow (itemId i)
                                   , "/", nm, "/set/"
                                   , tshow idx])
                         ,("data-box", "1")
                         ,("data-refresh", "page")]
                         (X.childNodes n)]
