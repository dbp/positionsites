{-# LANGUAGE OverloadedStrings  #-}

module Handler.Auth where

import           Control.Applicative
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.Monoid
import           Data.Maybe
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Heist
import qualified Heist.Interpreted as I
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Digestive.Heist

import           Application
import           Helpers.Text
import           Helpers.Forms

loginHandler = undefined
logoutHandler = undefined
resetHandler = undefined
forgotHandler = undefined

data SignupData = SignupData Text Text
signupForm :: Form Text AppHandler SignupData
signupForm = SignupData
  <$> "email" .: emailForm Nothing
  <*> "password" .: passwordForm

signupHandler = do
  r <- runForm "signup" signupForm
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (SignupData email password)) -> do
     res <- with auth $ createUser email (T.encodeUtf8 password)
     case res of
       Left failure -> render' v  (Just (tshow failure))
       Right user ->
         redirect "/"
 where render' v msg = renderWithSplices "signup"
                       (digestiveSplices v <>
                        ("message" ## I.textSplice (fromMaybe "" msg)))
