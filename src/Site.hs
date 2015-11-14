{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Trans
import           Data.ByteString                             (ByteString)
import           Data.Monoid
import qualified Data.Text                                   as T
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Conduit                        (Manager,
                                                              closeManager, conduitManagerSettings,
                                                              newManager)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Text.XmlHtml                                as X

import           Application
import           Handler.Site                                (rebindSplice)
import           Helpers.Text
import           Routes                                      (routes)

serverSplices :: Splices (Splice AppHandler)
serverSplices = do "server-port" ## serverPortSplice
                   "psInput" ## psInputSplice
                   "rebind" ## rebindSplice

serverPortSplice :: Splice AppHandler
serverPortSplice = do
  port <- lift (fmap rqServerPort getRequest)
  return (if port == 80 || port == 0
             then []
             else [X.TextNode (T.append ":" $ tshow port)])

psInputSplice :: Splice AppHandler
psInputSplice = do n <- getParamNode
                   let t = X.getAttribute "type" n
                   let r = X.getAttribute "ref" n
                   case (,) <$> t <*> r of
                     Nothing -> error "psInput: need 'type' attribute"
                     Just ("text", ref) ->  return [X.Element "dfInputTextArea"
                                                              [("ref", ref)]
                                                              []]
                     Just ("checkbox", ref) ->  return [X.Element "dfInputCheckbox"
                                                                 [("ref", ref)]
                                                                 []]
                     Just ("list", ref) -> return [X.TextNode "Add elements later."
                                                  ,X.Element "dfInputHidden"
                                                                   [("ref", ref)]
                                                                   []]
                     Just ("data", ref) -> return [X.TextNode "Set this later."
                                                 ,X.Element "dfInputHidden"
                                                                  [("ref", ref)]
                                                                  []]
                     Just ("hidden", ref) -> return [X.Element "dfInputHidden"
                                                               [("ref", ref)]
                                                               []]
                     Just ("file", ref) -> return [X.Element "dfInputFile"
                                                             [("ref", ref)]
                                                             []]

app :: SnapletInit App App
app = makeSnaplet "app" "An data-driven CMS." Nothing $ do
    let appHeistConfig = set hcLoadTimeSplices defaultLoadTimeSplices $
                         set hcInterpretedSplices serverSplices $
                         emptyHeistConfig
    h <- nestSnaplet "" heist $
         heistInit' "templates" appHeistConfig
    s <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess"
          (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    man <- liftIO (newManager conduitManagerSettings)
    onUnload (closeManager man)
    addRoutes (routes man)
    addAuthSplices h auth
    return $ App h s a d
