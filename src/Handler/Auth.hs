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
udLogin :: UserData -> Text
udLogin (UserData l _) = l

userForm :: Formlet Text AppHandler UserData
userForm u = UserData
  <$> "username" .: (T.toLower <$> nonEmpty (text (fmap udLogin u)))
  <*> "password" .: passwordForm

data EditUserData = EditUserData {eudU :: Text, eudP :: Text, eudA :: Bool}

editUserForm :: Formlet Text AppHandler EditUserData
editUserForm u = EditUserData
  <$> "username" .: (T.toLower <$> nonEmpty (text (fmap eudU u)))
  <*> "password" .: text Nothing
  <*> "admin"    .: bool (fmap eudA u)

loginHandler :: AppHandler ()
loginHandler = do
  r <- runForm "login" (userForm Nothing)
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (UserData username password)) -> do
      res <- with auth $ loginByUsername username (ClearText (T.encodeUtf8 password)) False
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
  r <- runForm "signup" (userForm Nothing)
  case r of
    (v, Nothing) -> render' v Nothing
    (v, Just (UserData username password)) -> do
     res <- with auth $ createUser username (T.encodeUtf8 password)
     case res of
       Left failure -> render' v  (Just (tshow failure))
       Right user -> redirect "/"
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
