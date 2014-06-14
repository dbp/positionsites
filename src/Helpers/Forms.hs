{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Helpers.Forms where

import Snap.Core
import Control.Monad.Trans (liftIO)
import Data.Traversable (sequenceA)
import Data.Map (Map)
import Data.Aeson hiding (Error, Success, (.:))
import Text.Digestive
import Text.Digestive.Snap
import Snap.Util.FileUploads
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Text (Text)
import Text.XmlHtml


import Application
import Helpers.Text
import State.User (SiteUser)
import State.Site (Site)
import State.Data (Item(..), FieldSpec(..), FieldData, parseSpec, renderFieldData,
                   boolFieldData, shortName)

runForm' :: MonadSnap m	 => Text -> Form v m a -> m (View v, Maybe a)
runForm' = runFormWith (defaultSnapFormConfig { uploadPolicy = setMaximumFormInputSize tenmegs defaultUploadPolicy
                                              , partPolicy = const $ allowWithMaximumSize tenmegs})
  where tenmegs = 10 * 1024 * 1024

emailForm :: Maybe Text -> Form Text AppHandler Text
emailForm email = check "Not a valid email address." (\e -> T.isInfixOf "@" e) $
                  nonEmpty (text email)

passwordForm :: Form Text AppHandler Text
passwordForm = nonEmptyTextForm

nonEmpty :: Form Text AppHandler Text -> Form Text AppHandler Text
nonEmpty = check "Must not be blank" tNotNull

nonEmptyTextForm :: Form Text AppHandler Text
nonEmptyTextForm = nonEmpty (text Nothing)

noSpaces :: Form Text AppHandler Text -> Form Text AppHandler Text
noSpaces = check "Cannot contain spaces" (\t -> not $ " " `T.isInfixOf` t)

jsonMapForm :: FromJSON a => Form Text AppHandler (Map Text a)
jsonMapForm = validate (\e -> case decode (LT.encodeUtf8 $ LT.fromStrict e) of
                                Nothing -> Error "Not a valid JSON map."
                                Just m -> Success m)
              nonEmptyTextForm

fieldSpecForm :: Form Text AppHandler FieldSpec
fieldSpecForm = validate (\e -> case decode (LT.encodeUtf8 $ LT.fromStrict $ T.concat ["[", e, "]"]) of
                                  Nothing -> Error "Not a valid field spec (remember quotes)."
                                  Just m -> Success (head m))
                nonEmptyTextForm

fieldsForm :: Site -> [(Text, FieldSpec)] -> Form Text AppHandler [(Text, FieldData)]
fieldsForm site = sequenceA . map (($ Nothing) . uncurry (fieldForm site))

fieldForm :: Site -> Text -> FieldSpec -> Maybe FieldData -> Form Text AppHandler (Text, FieldData)
fieldForm site n spec d = n .: validateM (\f -> do r <- parseSpec site spec f
                                                   case r of
                                                     Nothing ->
                                                       return (Error (T.concat ["Not a valid "
                                                                               , n
                                                                               , "."]))
                                                     Just f -> return (Success (n, f)))
                                          (case spec of
                                             ImageFieldSpec -> imageForm
                                             BoolFieldSpec -> validate (Success . tshow) $
                                               bool (fmap boolFieldData d)
                                             _ -> text (fmap renderFieldData d))



fieldDataExistingForm :: [Item] -> Form Text AppHandler Int
fieldDataExistingForm items = "item" .: choice (map (\i -> (itemId i, shortName i)) items) Nothing

validateHtml :: Form Text AppHandler Text -> Form Text AppHandler Text
validateHtml = validate (\x -> case parseHTML "" (T.encodeUtf8 x) of
                                 Left err -> Error $ T.pack err
                                 Right _ -> Success x)


imageForm :: Form Text AppHandler Text
imageForm = validate required file
  where required Nothing = Error "File is required."
        required (Just p) = Success (T.pack p)


ownershipForm :: [(SiteUser, Text)] -> Formlet Text AppHandler SiteUser
ownershipForm users cur = "owner" .: choice users cur
