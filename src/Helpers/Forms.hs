{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Helpers.Forms where

import Data.Map (Map)
import Data.Aeson hiding (Error, Success)
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Text (Text)

import Application
import Helpers.Text

emailForm :: Maybe Text -> Form Text AppHandler Text
emailForm email = check "Not a valid email address." (\e -> T.isInfixOf "@" e) $
                  check "Must not be blank" tNotNull
                  (text email)

passwordForm :: Form Text AppHandler Text
passwordForm = nonEmptyTextForm

nonEmptyTextForm :: Form Text AppHandler Text
nonEmptyTextForm = check "Must not be blank" tNotNull (text Nothing)

jsonMapForm :: FromJSON a => Form Text AppHandler (Map Text a)
jsonMapForm = validate (\e -> case decode (LT.encodeUtf8 $ LT.fromStrict e) of
                                Nothing -> Error "Not a valid JSON map."
                                Just m -> Success m)
              nonEmptyTextForm
