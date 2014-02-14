{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections  #-}

module Handler.Site where

import Prelude hiding (lookup)
import qualified Prelude as L (lookup)
import Control.Applicative hiding (empty)
import Data.Map (Map, assocs, fromList, lookup, insert, (!), empty)
import Data.Monoid
import Data.Maybe
import Data.List (intersperse)
import Snap.Core
import Snap.Snaplet
import Snap.Util.FileServe
import Heist
import Heist.Splices.Html
import Heist.Interpreted (Splice, textSplice, addTemplate
                         ,renderTemplate, bindSplices, bindSplice
                         ,runChildrenWith, lookupSplice)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import "mtl" Control.Monad.Trans
import Control.Monad.Trans.Either
import Text.XmlHtml hiding (render)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Text.Digestive
import Text.Digestive.Snap hiding (method)
import Text.Digestive.Heist
import Heist.Splices.BindStrict
import Web.Analyze.Client
import Network.HTTP.Conduit (Manager)

import Application
import State.Site
import State.Page
import State.Data
import State.Image
import State.User
import State.HeaderFile
import State.Blob
import Splice.Data
import Splice.Page
import Splice.HeaderFile
import Splice.Blob
import Splice.User
import Helpers.Forms
import Helpers.Misc
import Helpers.Text
import Helpers.State
import Handler.API
import Handler.Auth

sitePath :: Site -> ByteString
sitePath (Site id' _ _ _) = B.append "/site/" (B8.pack (show id'))


siteForm :: Maybe Site -> Form Text AppHandler (Text, Text, Text)
siteForm old = (,,) <$> "base" .: validateHtml (nonEmpty (text (fmap siteBase old)))
                    <*> "domain" .: nonEmpty (text (fmap siteUrl old))
                    <*> "token" .: text (siteAnalyzeToken =<< old)

renderError :: AppHandler ()
renderError = render "error"

newSiteHandler :: AppHandler ()
newSiteHandler = do
  r <- runForm "new-site" (siteForm (Just (Site (-1) "" "<authlink/>\n\n<apply-content/>" Nothing)))
  case r of
    (v, Nothing) -> renderWithSplices "site/new" (digestiveSplices v)
    (_, Just (base, url, token)) -> do
      mid <- newSite (Site (-1) url base (if token == "" then Nothing else Just token))
      case mid of
        Nothing -> error "Site could not be created"
        Just site_id -> redirect (sitePath (Site site_id "" "" Nothing))

manageSiteHandler :: AppHandler ()
manageSiteHandler = do
  mid <- getParam "id"
  case fmap B8.unpack mid >>= readSafe of
    Nothing -> pass
    Just id' -> do
      msite <- getSiteById id'
      case msite of
        Nothing -> pass
        Just site ->
          route [("", ifTop $ showSiteHandler site)
                ,("/edit", editSiteHandler site)
                ,("/data/new", newDataHandler site)
                ,("/page/new", newPageHandler site)
                ,("/page/edit/:id", editPageHandler site)
                ,("/header/new", newHeaderHandler site)
                ,("/header/edit/:id", editHeaderHandler site)
                ,("/header/delete/:id", deleteHeaderHandler site)
                ,("/user/new", newUserHandler site)
                ,("/blob/new", newBlobHandler site)
                ,("/blob/edit/:id", editBlobHandler site)
                ]

showSiteHandler :: Site -> AppHandler ()
showSiteHandler site = do
  ds <- getSiteData site
  pgs <- getSitePages site
  hdrs <- getSiteHeaders site
  blobs <- getSiteBlobs site
  users <- getSiteUsers site
  renderWithSplices "site/index" $ do
    "site_id" ## textSplice (tshow (siteId site))
    "domain" ## textSplice (siteUrl site)
    "users" ## manageUsersSplice users
    "data" ## manageDataSplice ds
    "pages" ## managePagesSplice pgs
    "headers" ## manageHeadersSplice hdrs
    "blobs" ## manageBlobsSplice blobs

editSiteHandler :: Site -> AppHandler ()
editSiteHandler site = do
  r <- runForm "edit-base" (siteForm (Just site))
  case r of
    (v, Nothing) -> renderWithSplices "site/edit" (digestiveSplices v)
    (_, Just (base, domain, token)) -> do
      updateSite (site { siteBase = base, siteUrl = domain
                       , siteAnalyzeToken = if token == "" then Nothing else Just token })
      redirect (sitePath site)

newGenHandler :: Site
               -> Form Text AppHandler a
               -> ByteString
               -> (a -> AppHandler ())
               -> AppHandler ()
newGenHandler site form tmpl creat = do
  r <- runForm "new-gen" form
  case r of
    (v, Nothing) -> renderWithSplices tmpl (digestiveSplices v)
    (_, Just x) -> do
      creat x
      redirect (sitePath site)

editGenHandler :: Site
               -> (Int -> Site -> AppHandler (Maybe a))
               -> Formlet Text AppHandler a
               -> ByteString
               -> (a -> AppHandler ())
               -> AppHandler ()
editGenHandler site getter form tmpl updt = do
  mid <- getParam "id"
  case bsId mid of
    Nothing -> pass
    Just id' -> do
      mo <- getter id' site
      case mo of
        Nothing -> pass
        Just obj -> do
          r <- runForm "edit-gen" $ form (Just obj)
          case r of
            (v, Nothing) -> renderWithSplices tmpl (digestiveSplices v)
            (_, Just x) -> do
              updt x
              redirect (sitePath site)

deleteGenHandler :: Site
                 -> (Int -> Site -> AppHandler ())
                 -> AppHandler ()
deleteGenHandler site dlt = do
  mid <- getParam "id"
  case bsId mid of
    Nothing -> pass
    Just id' -> do dlt id' site
                   redirect (sitePath site)

newDataForm :: Form Text AppHandler (Text, Map Text FieldSpec)
newDataForm = (,) <$> "name"   .: nonEmptyTextForm
                  <*> "fields" .: jsonMapForm

newDataHandler :: Site -> AppHandler ()
newDataHandler site = newGenHandler site newDataForm "data/new" $
  \(name, fields) -> void $ newData (Data (-1) (siteId site) name fields)

pageForm :: Site -> Maybe Page -> Form Text AppHandler Page
pageForm site p = mkPg <$> "flat" .: nonEmpty (text $ fmap (decodeUtf8 . pageFlat) p)
                       <*> "structured" .: nonEmpty (text $ fmap pageStructured p)
                       <*> "body" .: validateHtml (nonEmpty (text $ fmap pageBody p))
  where mkPg f s b = case p of
                       Nothing -> Page (-1) (siteId site) (encodeUtf8 f) s b
                       Just pg -> pg { pageFlat = encodeUtf8 f, pageStructured = s, pageBody = b }

newPageHandler :: Site -> AppHandler ()
newPageHandler site = newGenHandler site (pageForm site Nothing) "page/new" (void . newPage)

editPageHandler :: Site -> AppHandler ()
editPageHandler site = editGenHandler site getPageById (pageForm site) "page/edit" updatePage

headerForm :: Site -> Maybe HeaderFile -> Form Text AppHandler HeaderFile
headerForm site h = mkHeader <$> "name" .: nonEmpty (text $ fmap headerFileName h)
                             <*> "type" .: choice [ (HeaderCSS, "CSS")
                                                  , (HeaderJavascript, "Javascript")]
                                                  (fmap headerFileType h)
                             <*> "content" .: nonEmpty (text $ fmap headerFileContent h)
  where mkHeader n t c = case h of
                           Nothing -> HeaderFile (-1) (siteId site) t n c
                           Just header -> header { headerFileType = t
                                                 , headerFileName = n
                                                 , headerFileContent = c
                                                 }

newHeaderHandler :: Site -> AppHandler ()
newHeaderHandler site = newGenHandler site (headerForm site Nothing) "header/new" (void . newHeader)

editHeaderHandler :: Site -> AppHandler ()
editHeaderHandler site = editGenHandler site getHeaderById (headerForm site) "header/edit" updateHeader

deleteHeaderHandler :: Site -> AppHandler ()
deleteHeaderHandler site = deleteGenHandler site deleteHeaderFile

blobForm :: Site -> Maybe Blob -> Form Text AppHandler Blob
blobForm site b = mkBlob <$> "name" .: nonEmpty (text (fmap blobName b))
                         <*> "type" .: choice [(BlobPlain, "Plain Text")
                                              ,(BlobMarkdown, "Markdown")
                                              ,(BlobHTML, "HTML")] (fmap blobType b)
                         <*> "admin" .: bool (fmap blobAdmin b)
  where mkBlob n t a = case b of
                         Nothing -> Blob (-1) (siteId site) n "" t a
                         Just blob -> blob { blobName = n
                                           , blobType = t
                                           , blobAdmin = a
                                           }

newBlobHandler :: Site -> AppHandler ()
newBlobHandler site = newGenHandler site (blobForm site Nothing) "blob/new" (void . newBlob)

editBlobHandler :: Site -> AppHandler ()
editBlobHandler site = editGenHandler site getBlobById (blobForm site) "blob/edit" updateBlob

newUserHandler :: Site -> AppHandler ()
newUserHandler site = newGenHandler site (userForm Nothing) "user/new" $
  \(UserData login pass) -> do mu <- with auth $ createUser login (encodeUtf8 pass)
                               case mu of
                                 Left err -> return ()
                                 Right au ->
                                   case userId au of
                                     Nothing -> return ()
                                     Just uid ->
                                       newUser (SiteUser (read $ T.unpack $ unUid uid)
                                                         (siteId site)
                                                         False)


-- What follows is routing the frontend of the site, ie when accessed from the
-- site's domain.

loginGuard :: AppHandler () -> AppHandler ()
loginGuard hndlr = do
  li <- with auth isLoggedIn
  if li
     then hndlr
     else do
       modifyResponse (setResponseCode 401)
       return ()

loginGuard' :: (SiteUser -> AppHandler ()) -> AppHandler ()
loginGuard' hndlr = do
  u <- with auth currentUser
  case u >>= userId of
    Nothing -> forbidden
    Just id' -> do
      su <- getUser (read (T.unpack (unUid id')))
      case su of
        Nothing -> forbidden
        Just siteuser ->
          hndlr siteuser

siteHandler :: Manager -> Site -> AppHandler ()
siteHandler man site =
  siteWrap $ route [("/api", loginGuard' $ siteApiHandler site)
                   ,("/login", loginHandler)
                   ,("/logout", logoutHandler)
                   ,("/signup", signupHandler)
                   ,("/images/:name", imagesHandler site)
                   ,("/header/:id", headerHandler site)
                   ,("", do pages <- getSitePages site
                            routePages site pages)]
  where siteWrap = case siteAnalyzeToken site of
                     Nothing -> id
                     Just token -> wrap renderError man (encodeUtf8 token)


imagesHandler :: Site -> AppHandler ()
imagesHandler site =
  do n <- getParam "name"
     case n of
       Nothing -> pass
       Just name ->
         do repo <- getImageRepository
            serveFile ((T.unpack repo) ++ "/" ++ (B8.unpack name))

headerHandler :: Site -> AppHandler ()
headerHandler site =
  do i <- getParam "id"
     case i >>= (readSafe . T.unpack . decodeUtf8) of
       Nothing -> pass
       Just id' -> do hf <- getHeaderById id' site
                      case hf of
                        Nothing -> pass
                        Just header ->
                          writeText (headerFileContent header)

routePages :: Site -> [Page] -> AppHandler ()
routePages site pgs =
  route (map (\p -> (pageFlat p, renderPage site p))
             pgs)

rebindSplice :: Splice AppHandler
rebindSplice = do
  node <- getParamNode
  let attrs = do o <- getAttribute "old" node
                 n <- getAttribute "new" node
                 return (o, n)
  case attrs of
    Nothing -> return []
    Just (old, new) -> do
      st <- getHS
      let spl = lookupSplice old st
      case spl of
        Nothing -> return []
        Just splice -> do
           modifyHS $ bindSplice new splice
           return []

authLinkSplice :: Splice AppHandler
authLinkSplice = do
  mau <- lift $ with auth currentUser
  u <- lift $ fmap rqURI getRequest
  let url = decodeUtf8 (urlEncode u)
  return (case mau of
            Just au -> [Element "a" [("class", "authlink ps-link")
                                    ,("href", T.append "/logout?redirect=" url)
                                    ,("title", userLogin au)
                                    ]
                                  [TextNode "Logout"]]
            Nothing -> [Element "a" [("class", "authlink ps-link")
                                    ,("href", T.append "/login?redirect=" url)
                                    ]
                                   [TextNode "Login"]])

headersSplice :: Site -> Splice AppHandler
headersSplice site = do
  hfs <- lift $ getSiteHeaders site
  return (map renderHeader hfs)
 where renderHeader hf = case headerFileType hf of
                           HeaderCSS -> Element "link"
                                          [("href",
                                           T.concat ["/header/", tshow (headerFileId hf),
                                                     "/src.css"])
                                          ,("rel", "stylesheet")
                                          ,("type", "text/css")] []
                           HeaderJavascript -> Element "script"
                                                       [("src",
                                                         T.concat ["/header/",
                                                                  tshow (headerFileId hf),
                                                                  "/src.js"])
                                                        ,("type", "text/javascript")] []


blobSplice :: Site -> Splice AppHandler
blobSplice site = do
  node <- getParamNode
  case "name" `L.lookup` (elementAttrs node) of
    Nothing -> return []
    Just name -> do
      b <- lift $ getBlobByName site name
      case b of
        Nothing -> return []
        Just blob -> return (renderBlob blob)
  where renderBlob b = case blobType b of
                         BlobPlain -> newlineReplace (blobContent b)
                         BlobHTML -> case parseHTML "" (encodeUtf8 $ blobContent b)  of
                                       Left err -> []
                                       Right html -> docContent html
                         BlobMarkdown -> error "Don't support markdown yet."

setBlobSplice :: Site -> Splice AppHandler
setBlobSplice site = do
  node <- getParamNode
  case "name" `L.lookup` (elementAttrs node) of
    Nothing -> return []
    Just name -> do
      b <- lift $ getBlobByName site name
      case b of
        Nothing -> return []
        Just blob ->
          if blobAdmin blob
             -- force an admin check
             then loginGuardSplice' (Item (-1) (-1) (-1) (-1) empty) $ do
               linkSplice editPoint (T.concat ["/api/blob/set/", tshow (blobId blob)])
             -- just a regular login check
             else loginGuardSplice $ do
               linkSplice editPoint (T.concat ["/api/blob/set/", tshow (blobId blob)])


isUrlSplice :: Splice AppHandler
isUrlSplice = do node <- getParamNode
                 case getAttribute "url" node of
                   Nothing -> return []
                   Just u -> do url <- fmap rqURI getRequest
                                if u == (decodeUtf8 url)
                                   then return (elementChildren node)
                                   else return []
prefixUrlSplice :: Splice AppHandler
prefixUrlSplice = do node <- getParamNode
                     case getAttribute "url" node of
                       Nothing -> return []
                       Just u -> do url <- fmap rqURI getRequest
                                    if u `T.isPrefixOf` (decodeUtf8 url)
                                      then return (elementChildren node)
                                      else return []

siteSplices :: Site ->  Splices (Splice AppHandler)
siteSplices site = do "rebind" ## rebindSplice
                      "authlink" ## authLinkSplice
                      "html" ## htmlImpl
                      "headers" ## headersSplice site
                      "blob" ## blobSplice site
                      "set-blob" ## setBlobSplice site
                      "is-url" ## isUrlSplice
                      "prefix-url" ## prefixUrlSplice
                      bindStrictTag ## bindStrictImpl

renderPage :: Site -> Page -> AppHandler ()
renderPage s p = do
  urlDataSplices <- fmap mconcat (mapM (loadData s) (zip (T.splitOn "/" (decodeUtf8 (pageFlat p))) (T.splitOn "/" (pageStructured p))))
  ds <- getSiteData s
  let splices = (mconcat $ map (dataSplices s) ds) <> siteSplices s
  modifyResponse (setContentType "text/html")
  case parseHTML "" (encodeUtf8 $ pageBody p) of
    Left err -> error (show err)
    Right html -> do
      st <- fmap (either (error.show) id) $
        liftIO $ runEitherT $ initHeist $ mempty { hcTemplateLocations =
                                                   [loadTemplates "snaplets/heist/templates/sites"]
                                                 }
      let newst = addTemplate "site_base" (docContent
                  (fromRight (parseHTML "" (encodeUtf8 $ siteBase s)))) Nothing st
      let newst' = addTemplate "page" [Element "apply" [("template", "site")] (docContent html)]
                   Nothing newst
      let newst'' = bindSplices (urlDataSplices <> splices <> defaultLoadTimeSplices) newst'

      res <- renderTemplate newst'' "page"
      case res of
        Nothing -> error "Could not render template"
        Just (builder, _) -> writeBuilder builder

loadData :: Site -> (Text, Text) -> AppHandler (Splices (Splice AppHandler))
loadData site (f, s) | T.isPrefixOf "id(" s && T.isSuffixOf ")" s && T.isPrefixOf ":" f = do
  mparam <- getParam (encodeUtf8 (T.drop 1 f))
  case bsId mparam of
    Nothing -> passLog' ["Param missing or not an integer: ", f]
    Just id' -> do
      let name = fromJust $ (T.stripSuffix ")") =<< (T.stripPrefix "id(" s)
      mdat <- getDataByName site name
      case mdat of
        Nothing -> error $ "Unknown data: " ++ (T.unpack name)
        Just dat -> do
          mitem <- getItemById site id'
          case mitem of
            Nothing -> passLog' ["Id for item does not correspond to an item: ", tshow id']
            Just item ->
              case itemDataId item == dataId dat of
                False -> passLog' ["Id specified does not correspond to the right data type: ", tshow id', " for data ", name]
                True ->
                  return $ T.append "this-" name ## runChildrenWith (itemSplices site dat item)
 where passLog' a = passLog a >> return mempty
loadData _ _ = return mempty
