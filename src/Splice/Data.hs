{-# LANGUAGE OverloadedStrings, PackageImports  #-}

module Splice.Data where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import "mtl" Control.Monad.Trans (lift, liftIO)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import qualified Text.XmlHtml as X
import Heist
import Heist.Interpreted
import Heist.Splices
import Application
import State.Data
import State.Site
import State.Image
import State.User
import Helpers.Text
import Helpers.Misc


loginGuardSplice :: Splice AppHandler -> Splice AppHandler
loginGuardSplice s = do
 li <- lift $ with auth isLoggedIn
 if li
    then s
    else return []

loginGuardSplice' :: Item -> Splice AppHandler -> Splice AppHandler
loginGuardSplice' item s = do
 mau <- lift $ with auth currentUser
 case mau >>= userId >>= (readSafe . T.unpack . unUid) of
   Nothing -> return []
   Just id' -> do
     mu <- lift $ getUser id'
     case mu of
       Nothing -> return []
       Just u ->
         if siteUserAdmin u || siteUserId u == itemOwnerId item
                   then s
                   else return []

loginAdminGuardSplice :: Splice AppHandler -> Splice AppHandler
loginAdminGuardSplice s = do
 mau <- lift $ with auth currentUser
 case mau >>= userId >>= (readSafe . T.unpack . unUid) of
   Nothing -> return []
   Just id' -> do
     mu <- lift $ getUser id'
     case mu of
       Nothing -> return []
       Just u ->
         if siteUserAdmin u
                   then s
                   else return []


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

apiFieldsSplice :: Data -> [(Text, FieldSpec)] -> Splices (Splice AppHandler)
apiFieldsSplice d vs = "fields" ## mapSplices (runChildrenWith . fieldsSplice) vs

fieldsSplice :: (Text, FieldSpec) -> Splices (Splice AppHandler)
fieldsSplice (n, StringFieldSpec) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "text"
fieldsSplice (n, NumberFieldSpec) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "text"
fieldsSplice (n, ImageFieldSpec) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "file"
fieldsSplice (n, ListFieldSpec et) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "list"
fieldsSplice (n, DataFieldSpec name) = do
  "field-ref" ## textSplice n
  "field-type" ## textSplice "data"

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
  T.append "new-" (dataName d) ## loginGuardSplice $ newItemSplice d

data SortOrder = ASC | DESC deriving (Eq, Show, Read)

renderAllItems :: Site -> Data -> Splice AppHandler
renderAllItems s d = do
  sort <- fmap (lookup "sort" . X.elementAttrs) getParamNode
  items <- lift $ getItems d
  let sorted = case sort of
                 Nothing -> items
                 Just s -> case T.splitOn ":" s  of
                             (f:[]) -> sortItems f DESC items
                             (f:o:[]) -> sortItems f (read (T.unpack o)) items
  mapSplices (runChildrenWith . itemSplices s d) sorted
  where sortItems fld DESC = sortBy (comparing ((M.! fld) . itemFields))
        sortItems fld ASC = sortBy (flip (comparing ((M.! fld) . itemFields)))

itemSplices :: Site -> Data -> Item
            -> Splices (Splice AppHandler)
itemSplices s d i = ("delete" ## loginGuardSplice' i $ deleteSplice d i)
                    <> fieldsSplices s d i (M.assocs $ dataFields d)

fieldsSplices :: Site -> Data -> Item -> [(Text, FieldSpec)] -> Splices (Splice AppHandler)
fieldsSplices s d i fields = (do "id" ## textSplice (tshow (itemId i))
                                 "ownership" ## loginAdminGuardSplice (ownershipSplice i)) <>
 (mconcat $ map (\(name, spec) -> do
              name ## fldSplice spec s d i name (M.lookup name (itemFields i))
              case spec of
                ListFieldSpec (DataFieldSpec _) -> do
                  T.concat ["add-", name, "-existing"] ## loginGuardSplice' i $ addListFieldDataExistingSplice i name
                  T.concat ["add-", name, "-new"] ## loginGuardSplice' i $ addListFieldDataNewSplice i name
                ListFieldSpec _ ->
                  T.concat ["add-",  name] ## loginGuardSplice' i $ addListFieldSplice i name
                ImageFieldSpec ->
                  T.concat ["set-", name] ## loginGuardSplice' i $ imageSetFieldSplice i name
                _ -> T.concat ["set-", name] ## loginGuardSplice' i $ setFieldSplice i name)
            fields)

fldSplice :: FieldSpec -> Site -> Data -> Item -> Text -> Maybe FieldData -> Splice AppHandler
fldSplice _ _ _ _ _ Nothing = textSplice ""
fldSplice _ _ _ _ _ (Just (StringFieldData s)) = return (newlineReplace s)
fldSplice _ _ _ _ _ (Just (NumberFieldData n)) = textSplice (tshow n)
fldSplice _ _ _ _ _ (Just (ImageFieldData id')) = imageSplice id'
fldSplice (ListFieldSpec (DataFieldSpec nm)) s d i n (Just (ListFieldData ls)) =
  let len = length ls
  in
  mapSplices (runChildrenWith .
              (\(Just (idx, f)) -> do
                "delete" ## loginGuardSplice' i $ deleteListFieldSplice i n idx
                "set-existing" ## loginGuardSplice' i $ setListFieldDataExistingSplice i n idx
                "set-new" ## loginGuardSplice' i $ setListFieldDataNewSplice i n idx
                "element" ## fldSplice (DataFieldSpec nm) s d i n (Just f)
                if idx /= 0
                   then "prev" ## loginGuardSplice' i $ swapListItemSplice i n idx (idx - 1)
                   else "prev" ## ifISplice False
                if idx /= len - 1
                   then "next" ## loginGuardSplice' i $ swapListItemSplice i n idx (idx + 1)
                   else "next" ## ifISplice False) .
              Just) (zip [0..] ls)
fldSplice (ListFieldSpec inner) s d i n (Just (ListFieldData ls)) =
  let len = length ls
  in
  mapSplices (runChildrenWith .
             (\(Just (idx, f)) -> do
               "delete" ## loginGuardSplice' i $ deleteListFieldSplice i n idx
               "set" ## loginGuardSplice' i $ setListFieldSplice i n idx
               "element" ## fldSplice inner s d i n (Just f)
               if idx /= 0
                  then "prev" ## loginGuardSplice' i $ swapListItemSplice i n idx (idx - 1)
                  else "prev" ## ifISplice False
               if idx /= len - 1
                  then "next" ## loginGuardSplice' i $ swapListItemSplice i n idx (idx + 1)
                  else "next" ## ifISplice False) .
             Just) (zip [0..] ls)
fldSplice _ s d i name (Just (DataFieldData mid)) =
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
                (fieldsSplices s dat item (M.assocs (dataFields dat))) <>
                (shared True) <>
                ("delete" ## loginGuardSplice' i $ deleteDataFieldSplice i name)
 where shared e = do "exists" ## ifISplice e
                     "set-existing" ## loginGuardSplice' i $ setDataFieldExistingSplice i name
                     "set-new" ## loginGuardSplice' i $ setDataFieldNewSplice i name


-- TODO(dbp 2014-01-27): make this MUCH safer.
imageSplice :: Int -> Splice AppHandler
imageSplice id' = do node <- getParamNode
                     case node of
                       (X.Element _ attrs _) ->
                         case lookup "size" attrs of
                           Nothing -> return []
                           Just size ->
                             do let (w, h) = if T.isInfixOf "x" size
                                      then let ps = T.splitOn "x" size
                                           in (read $ T.unpack (ps !! 0), read $ T.unpack (ps !! 1))
                                      else (read $ T.unpack size, read $ T.unpack size) :: (Int, Int)
                                im <- lift $ getImageById id'
                                case im of
                                  Nothing -> return []
                                  Just image -> do
                                    let url = getImageSizePath image w h
                                    let (width, height) = fixSizes (imageOrigWidth image) (imageOrigHeight image) w h
                                    return [X.Element "img" [ ("src", url)
                                                            , ("width", tshow width)
                                                            , ("height", tshow height)] []]

linkSplice :: Text -> Text -> Splice AppHandler
linkSplice char = linkSplice' [X.TextNode char]

linkSplice' :: [X.Node] -> Text -> Splice AppHandler
linkSplice' nodes lnk =
  return [X.Element "a" [("href", lnk)
                       ,("class", "ps-link")
                       ,("data-box", "1")
                       ,("data-refresh", "page")]
                       nodes]

actionSplice :: [X.Node] -> Text -> Splice AppHandler
actionSplice nodes lnk =
  return [X.Element "a" [("href", lnk)
                      ,("class", "ps-link")
                      ,("data-action", "1")
                      ,("data-refresh", "page")]
                      nodes]

editPoint :: Text
editPoint = "\9998"

addPoint :: Text
addPoint = "+"

deletePoint :: Text
deletePoint = "\215"

getOrderedParam :: HeistT n AppHandler Text
getOrderedParam = do n <- getParamNode
                     let mo = lookup "order" (X.elementAttrs n)
                     return $ case mo of
                               Nothing -> ""
                               Just o' -> T.append "?order=" o'

newItemSplice :: Data -> Splice AppHandler
newItemSplice d = do
   o <- getOrderedParam
   linkSplice addPoint (T.concat ["/api/new/", tshow (dataId d), o])

deleteSplice :: Data -> Item -> Splice AppHandler
deleteSplice d i = linkSplice deletePoint (T.concat ["/api/delete/", tshow (itemId i)])

setFieldSplice :: Item -> Text -> Splice AppHandler
setFieldSplice i nm = linkSplice editPoint (T.concat ["/api/set/", tshow (itemId i), "/", nm])

addListFieldSplice :: Item -> Text -> Splice AppHandler
addListFieldSplice i nm = linkSplice addPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/add"])

addListFieldDataExistingSplice :: Item -> Text -> Splice AppHandler
addListFieldDataExistingSplice i nm = linkSplice addPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/add/data/existing"])

addListFieldDataNewSplice :: Item -> Text -> Splice AppHandler
addListFieldDataNewSplice i nm = do
  o <- getOrderedParam
  linkSplice addPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/add/data/new", o])

deleteListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
deleteListFieldSplice i nm idx = linkSplice deletePoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/delete/", tshow idx])

setListFieldSplice :: Item -> Text -> Int -> Splice AppHandler
setListFieldSplice i nm idx = linkSplice editPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/set/", tshow idx])

setListFieldDataExistingSplice :: Item -> Text -> Int -> Splice AppHandler
setListFieldDataExistingSplice i nm idx = linkSplice editPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/set/", tshow idx, "/data/existing"])

setListFieldDataNewSplice :: Item -> Text -> Int -> Splice AppHandler
setListFieldDataNewSplice i nm idx = do
  o <- getOrderedParam
  linkSplice addPoint (T.concat ["/api/list/", tshow (itemId i), "/", nm, "/set/", tshow idx, "/data/new", o])

deleteDataFieldSplice :: Item -> Text -> Splice AppHandler
deleteDataFieldSplice i nm = linkSplice deletePoint (T.concat ["/api/delete/", tshow (itemId i), "/data/", nm])

setDataFieldExistingSplice :: Item -> Text -> Splice AppHandler
setDataFieldExistingSplice i nm = linkSplice editPoint (T.concat ["/api/set/", tshow (itemId i), "/data/", nm, "/existing"])

setDataFieldNewSplice :: Item -> Text -> Splice AppHandler
setDataFieldNewSplice i nm = do
  o <- getOrderedParam
  linkSplice addPoint (T.concat ["/api/set/", tshow (itemId i), "/data/", nm, "/new", o])

imageSetFieldSplice :: Item -> Text -> Splice AppHandler
imageSetFieldSplice i nm = linkSplice editPoint (T.concat ["/api/set/", tshow (itemId i), "/image/", nm])

swapListItemSplice :: Item -> Text -> Int -> Int -> Splice AppHandler
swapListItemSplice i nm idxa idxb =
  do n <- getParamNode
     actionSplice (X.elementChildren n)
      (T.concat ["/api/list/"
                , tshow (itemId i)
                , "/"
                , nm
                , "/swap/"
                , tshow idxa
                , "/"
                , tshow idxb
                ])

ownershipSplice :: Item -> Splice AppHandler
ownershipSplice i = do
  n <- getParamNode
  linkSplice' (X.elementChildren n) (T.concat ["/api/ownership/", tshow (itemId i)])
