{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app ) where


import           Control.Monad.Trans
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Monoid
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Text.XmlHtml as X
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Conduit (Manager, newManager, closeManager, conduitManagerSettings)

import           Application
import           Routes (routes)
import           Helpers.Text

serverSplices :: Splices (Splice AppHandler)
serverSplices = "server-port" ## serverPortSplice

serverPortSplice :: Splice AppHandler
serverPortSplice = do
  port <- lift (fmap rqServerPort getRequest)
  return (if port == 80 || port == 0
             then []
             else [X.TextNode (T.append ":" $ tshow port)])

app :: SnapletInit App App
app = makeSnaplet "app" "An data-driven CMS." Nothing $ do
    let defaultHeistConfig = mempty {
        hcLoadTimeSplices = defaultLoadTimeSplices
      , hcInterpretedSplices = serverSplices
      }
    h <- nestSnaplet "" heist $
         heistInit' "templates" defaultHeistConfig
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
