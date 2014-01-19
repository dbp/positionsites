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
import "mtl" Control.Monad.Trans (lift, liftIO)
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X
import Heist
import Heist.Interpreted
import Heist.Splices
import Application
import State.Data
import State.Site
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
fieldsSplice (n, DataFieldSpec name) = do
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


dataSplices :: Site -> Data -> Splices (Splice AppHandler)
dataSplices s d = do
  T.append "all-" (dataName d) ## renderAllItems s d
  T.append "new-" (dataName d) ## newItemSplice d

renderAllItems :: Site -> Data -> Splice AppHandler
renderAllItems s d = do
  items <- lift $ getItems d
  mapSplices (runChildrenWith . itemSplices s d) items

itemSplices :: Site -> Data -> Item
            -> Splices (Splice AppHandler)
itemSplices s d i = (do T.concat ["delete-", dataName d] ## deleteSplice d i)
                    <> fieldsSplices s d i (M.assocs $ dataFields d)

fieldsSplices :: Site -> Data -> Item -> [(Text, FieldSpec)] -> Splices (Splice AppHandler)
fieldsSplices s d i fields = (T.concat ["id"] ## textSplice (tshow (itemId i))) <>
 (mconcat $
                               map (\(name, spec) -> do
                                       T.concat [dataName d, "-", name] ## fldSplice s d i name (M.lookup name (itemFields i))
                                       if isListFieldSpec spec
                                          then T.concat ["list-add-", dataName d, "-", name] ## addListFieldSplice i name
                                          else T.concat ["set-", dataName d, "-", name] ## setFieldSplice i name)
                                  fields)

fldSplice :: Site -> Data -> Item -> Text -> Maybe FieldData -> Splice AppHandler
fldSplice _ _ _ _ Nothing = textSplice ""
fldSplice _ _ _ _ (Just (StringFieldData s)) = textSplice s
fldSplice _ _ _ _ (Just (NumberFieldData n)) = textSplice (tshow n)
fldSplice s d i n (Just (ListFieldData ls)) =
  mapSplices (runChildrenWith .
              (\(Just (idx, f)) -> do
                "delete-element" ## deleteListFieldSplice i n idx
                "set-element" ## setListFieldSplice i n idx
                "element" ## fldSplice s d i n (Just f)) .
              Just) (zip [0..] ls)
fldSplice s d i name (Just (DataFieldData mid)) =
  case mid of
    Nothing -> runChildrenWith (shared False)
    Just id' -> do
      mitem <- lift $ getItemById s id'
      case mitem of
        Nothing -> runChildrenWith (shared False)
        Just item -> do
          mdat <- lift $ getDataById s (itemDataId item)
          case mdat of
            Nothing -> error "Item without associated data"
            Just dat -> do
              runChildrenWith $
                (fieldsSplices s d item (M.assocs (dataFields dat))) <>
                (shared True) <>
                ("delete" ## deleteDataFieldSplice i name)
 where shared e = do "exists" ## ifISplice e
                     "set-existing" ## setDataFieldExistingSplice i name
                     "set-new" ## setDataFieldNewSplice i name

linkSplice :: Text -> Splice AppHandler
linkSplice lnk = do
  n <- getParamNode
  return [X.Element "a" [("href", lnk)
                             ,("data-box", "1")
                             ,("data-refresh", "page")]
                             (X.childNodes n)]

newItemSplice :: Data -> Splice AppHandler
newItemSplice d = linkSplice (T.concat ["/api/new/", tshow (dataId d)])

deleteSplice :: Data -> Item -> Splice AppHandler
deleteSplice d i = linkSplice (T.concat ["/api/delete/", tshow (itemId i)])

setFieldSplice :: Item -> Text -> Splice AppHandler
setFieldSplice i nm = linkSplice (T.concat ["/api/set/", tshow (itemId i), "/", nm])

addListFieldSplice :: Item -> Text -> Splice AppHandler
addListFieldSplice i nm = linkSplice (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/add"])

deleteListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
deleteListFieldSplice i nm idx = linkSplice (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/delete/", tshow idx])

setListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
setListFieldSplice i nm idx = linkSplice (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/set/", tshow idx])

deleteDataFieldSplice :: Item -> Text -> Splice AppHandler
deleteDataFieldSplice i nm = linkSplice (T.concat ["/api/delete/", tshow (itemId i), "/data/", nm])

setDataFieldExistingSplice :: Item -> Text -> Splice AppHandler
setDataFieldExistingSplice i nm = linkSplice (T.concat ["/api/set/", tshow (itemId i), "/data/", nm, "/existing"])

setDataFieldNewSplice :: Item -> Text -> Splice AppHandler
setDataFieldNewSplice i nm = linkSplice (T.concat ["/api/set/", tshow (itemId i), "/data/", nm, "/new"])
