{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app ) where


import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Heist

import           Application
import           Routes (routes)

app :: SnapletInit App App
app = makeSnaplet "app" "An data-driven CMS." Nothing $ do
    let defaultHeistConfig = mempty {
      hcLoadTimeSplices = defaultLoadTimeSplices
      }
    h <- nestSnaplet "" heist $
         heistInit' "templates" defaultHeistConfig
    s <- nestSnaplet "sess" sess $
          initCookieSessionManager "site_key.txt" "sess"
          (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d
