{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Helpers.Forms where

import Data.Map (Map)
import Data.Aeson hiding (Error, Success, (.:))
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Text (Text)

import Application
import Helpers.Text
import State.Data (FieldSpec, FieldData, parseSpec, renderFieldData)

emailForm :: Maybe Text -> Form Text AppHandler Text
emailForm email = check "Not a valid email address." (\e -> T.isInfixOf "@" e) $
                  nonEmpty (text email)

passwordForm :: Form Text AppHandler Text
passwordForm = nonEmptyTextForm

nonEmpty :: Form Text AppHandler Text -> Form Text AppHandler Text
nonEmpty = check "Must not be blank" tNotNull

nonEmptyTextForm :: Form Text AppHandler Text
nonEmptyTextForm = nonEmpty (text Nothing)

jsonMapForm :: FromJSON a => Form Text AppHandler (Map Text a)
jsonMapForm = validate (\e -> case decode (LT.encodeUtf8 $ LT.fromStrict e) of
                                Nothing -> Error "Not a valid JSON map."
                                Just m -> Success m)
              nonEmptyTextForm

fieldForm :: Text -> FieldSpec -> Maybe FieldData -> Form Text AppHandler (Text, FieldData)
fieldForm n spec d = n .: validate (fmap (n,) .
                                    (maybe (Error $ T.concat ["Not a valid ", n, "."]) Success) .
                                    (parseSpec spec))
                                   (text (fmap renderFieldData d))
