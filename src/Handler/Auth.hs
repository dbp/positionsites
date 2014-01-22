{-# LANGUAGE OverloadedStrings  #-}

module Handler.Auth where

import           Control.Applicative
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
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
import           State.User

data UserData = UserData Text Text
userForm :: Form Text AppHandler UserData
userForm = UserData
  <$> "email" .: emailForm Nothing
  <*> "password" .: passwordForm

loginHandler :: AppHandler ()
loginHandler = do
  r <- runForm "login" userForm
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (UserData email password)) -> do
      res <- with auth $ loginByUsername email (ClearText (T.encodeUtf8 password)) False
      case res of
        Left failure -> render' v  (Just (tshow failure))
        Right user -> do
          redir <- (fmap (fromMaybe "/")) (getParam "redirect")
          redirect redir
 where render' v msg = renderWithSplices "login"
                       (digestiveSplices v <>
                        ("message" ## I.textSplice (fromMaybe "" msg)))

logoutHandler :: AppHandler ()
logoutHandler = do
  with auth logout
  redir <- (fmap (fromMaybe "/")) (getParam "redirect")
  redirect redir


resetHandler = undefined
forgotHandler = undefined


signupHandler :: AppHandler ()
signupHandler = do
  r <- runForm "signup" userForm
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (UserData email password)) -> do
     res <- with auth $ createUser email (T.encodeUtf8 password)
     case res of
       Left failure -> render' v  (Just (tshow failure))
       Right user -> do
         newUser (SiteUser (read $ T.unpack $ unUid (fromJust (userId user))) False)
         redirect "/"
 where render' v msg = renderWithSplices "signup"
                       (digestiveSplices v <>
                        ("message" ## I.textSplice (fromMaybe "" msg)))


requireAdmin :: AppHandler () -> AppHandler ()
requireAdmin hndlr = do
  mau <- with auth currentUser
  case mau of
    Nothing -> loginRedirect
    Just au -> do
      muser <- getUser (read $ T.unpack $ unUid (fromJust (userId au)))
      case muser of
        Nothing -> error "Could not find user corresponding to authuser."
        Just user ->
          if siteUserAdmin user
             then hndlr
             else loginRedirect


loginRedirect :: AppHandler ()
loginRedirect = do
  url <- fmap rqURI getRequest
  redirect $ B.concat ["/login", "?redirect=", urlEncode url]
