{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Helpers.Forms where

import Text.Digestive
import qualified Data.Text as T
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
