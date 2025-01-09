{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Control.Monad.Logger (LogSource)
import Control.Lens ((^?), to, folded, filtered, _2)

import Import.NoFoundation

import Data.Aeson.Lens (key, AsValue(_String))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.List.Safe as LS (head)
import qualified Data.Text as T (intercalate)
import qualified Data.Text.Encoding as TE

import Database.Esqueleto.Experimental
    ( selectOne, from, table, where_, val, select, unionAll_, not_
    , (^.)
    )
import qualified Database.Esqueleto.Experimental as E ((==.))
import Database.Persist.Sql (ConnectionPool, runSqlPool)

import qualified Network.Wreq as W (get, responseHeader, responseBody)

import Text.Email.Validate (emailAddress, localPart)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Julius (juliusFile)

import Yesod.Auth.HashDB (authHashDBWithForm)
import Yesod.Auth.Message
    ( defaultMessage, englishMessage, russianMessage
    , AuthMessage
      ( InvalidLogin, LoginTitle
      )
    )
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }


mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a


widgetTopbar :: Maybe (Route App,[(Text,Text)]) -- ^ Back button
             -> Text                            -- ^ Title 
             -> Text                            -- ^ Overlay id
             -> Maybe Text                      -- ^ Id of delete dialog
             -> Maybe (Route App)               -- ^ Edit button
             -> Widget
widgetTopbar backlink title idOverlay idDialogDelete editRoute = do
    stati <- reqGetParams <$> getRequest
    rndr <- getUrlRenderParams
    idDialogMainMenu <- newIdent
    $(widgetFile "widgets/topbar")


widgetAccount :: Widget
widgetAccount = do
    user <- maybeAuth
    $(widgetFile "widgets/account")


widgetSnackbar :: [(Text,Html)] -> Widget
widgetSnackbar msgs = $(widgetFile "widgets/snackbar")


widgetMainMenuTrigger :: Text -> Text -> Widget
widgetMainMenuTrigger idOverlay idDialogMainMenu = $(widgetFile "widgets/trigger")
    

widgetMainMenu :: Text -> Text -> Widget
widgetMainMenu idOverlay idDialogMainMenu = do
    curr <- getCurrentRoute
    idButtonMainMenuClose <- newIdent
    $(widgetFile "widgets/menu")



widgetTheme :: Widget
widgetTheme = $(widgetFile "widgets/theme")


widgetLang :: Route App -> Text -> Widget
widgetLang action backlink  = do
    
    language <- fromMaybe "en" . LS.head <$> languages
    
    idMenuLang <- newIdent
    idHiddenSelect <- newIdent
    idFormLang <- newIdent
    idInputBacklink <- newIdent
    
    $(widgetFile "widgets/lang")
  where
      resolveLang :: Lang -> AppMessage
      resolveLang "ru" = MsgLangRu
      resolveLang _ = MsgLangEn


postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField paramLang
    back <- runInputPost $ ireq urlField paramBacklink
    setLanguage lang
    redirect back


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        lang <- fromMaybe "en" . LS.head <$> languages
        msgr <- getMessageRender

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- addStylesheet $ StaticR css_bootstrap_css
                                    -- ^ generated from @Settings/StaticFiles.hs@
            $(widgetFile "default-layout")
          
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized ServiceWorkerR _ = return Authorized

    isAuthorized HomeR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized DocsR _ = setUltDestCurrent >> return Authorized
    
    isAuthorized LangR _ = return Authorized
    
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized WebAppManifestR _ = return Authorized
    
    isAuthorized (StaticR _) _ = return Authorized   


    isAuthorized (DataR (StandardDeleR _)) _ = isAdmin
    isAuthorized (DataR (StandardEditR _)) _ = isAdmin
    isAuthorized (DataR StandardNewR) _ = isAdmin
    isAuthorized (DataR (StandardR _)) _ = isAdmin
    isAuthorized (DataR StandardsR) _ = setUltDestCurrent >> isAdmin

    isAuthorized (DataR (PumpLayoutDeleR _)) _ = isAdmin
    isAuthorized (DataR (PumpLayoutEditR _)) _ = isAdmin
    isAuthorized (DataR PumpLayoutNewR) _ = isAdmin
    isAuthorized (DataR (PumpLayoutR _)) _ = isAdmin
    isAuthorized (DataR PumpLayoutsR) _ = setUltDestCurrent >> isAdmin

    isAuthorized (DataR (PumpClassDeleR _)) _ = isAdmin
    isAuthorized (DataR (PumpClassEditR _)) _ = isAdmin
    isAuthorized (DataR PumpClassNewR) _ = isAdmin
    isAuthorized (DataR (PumpClassR _)) _ = isAdmin
    isAuthorized (DataR PumpClassesR) _ = setUltDestCurrent >> isAdmin
    
    isAuthorized (DataR (PumpOrientationDeleR _)) _ = isAdmin
    isAuthorized (DataR (PumpOrientationEditR _)) _ = isAdmin
    isAuthorized (DataR PumpOrientationNewR) _ = isAdmin
    isAuthorized (DataR (PumpOrientationR _)) _ = isAdmin
    isAuthorized (DataR PumpOrientationsR) _ = setUltDestCurrent >> isAdmin
    
    isAuthorized (DataR (PumpTypeDeleR _)) _ = isAdmin
    isAuthorized (DataR (PumpTypeEditR _)) _ = isAdmin
    isAuthorized (DataR PumpTypeNewR) _ = isAdmin
    isAuthorized (DataR (PumpTypeR _)) _ = isAdmin
    isAuthorized (DataR PumpTypesR) _ = setUltDestCurrent >> isAdmin
    
    isAuthorized (DataR (AccountProfileR uid)) _ = isAuthenticatedSelf uid
    isAuthorized (DataR (AccountSettingsR uid)) _ = isAuthenticatedSelf uid

    
    isAuthorized (DataR (UserResetPasswordR _)) _ = isAdmin
    isAuthorized (DataR (UserDeleR _)) _ = isAdmin
    isAuthorized (DataR (UserEditR _)) _ = isAdmin
    isAuthorized (DataR UserNewR) _ = isAdmin
    isAuthorized (DataR (UserR _)) _ = isAdmin
    isAuthorized (DataR UsersR) _ = setUltDestCurrent >> isAdmin
    isAuthorized (DataR (UserPhotoR _)) _ = return Authorized
        
    
    

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    errorHandler :: ErrorResponse -> HandlerFor App TypedContent
    errorHandler NotFound = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPageNotFound
            idHeader <- newIdent
            idHeaderStart <- newIdent
            $(widgetFile "error/not-found")
        provideRep $ return $ object ["message" .= ("Page not found." :: Text)]
        provideRep $ return ("Page not found." :: Text)

    errorHandler (PermissionDenied msg) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgPermissionDenied
            msgr <- getMessageRender
            msgs <- getMessages
            idOverlay <- newIdent
            $(widgetFile "error/permission-denied")
        provideRep $ do
            msgr <- getMessageRender
            return $ object ["message" .= (msgr MsgPermissionDenied <> "Permission Denied. " <> msg)]
        provideRep $ return $ "Permission Denied. " <> msg

    errorHandler (InvalidArgs msgs) = selectRep $ do
        provideRep $ defaultLayout $ do
            setTitleI MsgInvalidArguments
            $(widgetFile "error/invalid-args")
        provideRep $ return $ object ["message" .= msgs]
        provideRep $ return $ T.intercalate ", " msgs

    errorHandler x = defaultErrorHandler x


getServiceWorkerR :: Handler TypedContent
getServiceWorkerR = TypedContent typeJavascript . toContent . $(juliusFile "static/js/sw.julius")
                    <$> getUrlRenderParams


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout w = liftHandler $ do
        defaultLayout $ do
            setTitleI MsgSignIn
            $(widgetFile "auth/layout")

    loginHandler :: AuthHandler App Html
    loginHandler = do
        app <- getYesod
        tp <- getRouteToParent
        rndr <- getUrlRender
        backlink <- fromMaybe (rndr HomeR) <$> lookupSession keyUtlDest
        let indexes = [1..] 
        authLayout $ do
            setTitleI LoginTitle
            idButtonBack <- newIdent
            $(widgetFile "auth/login")

    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate (Creds plugin ident extra) =  liftHandler $ case plugin of
      "google" -> do
          let name :: Maybe Text
              name = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "name" . _String
              
          let picture :: Maybe Text
              picture = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "picture" . _String
              
          let email :: Maybe Text
              email = extra ^? folded . filtered ((== "userResponse") . fst) . _2 . key "email" . _String

          case email of
              Just em -> do
                  Entity uid _ <- runDB $ upsert User { userEmail = em
                                                      , userPassword = Nothing
                                                      , userName = name
                                                      , userSuper = False
                                                      , userAdmin = False
                                                      , userAuthType = UserAuthTypeGoogle
                                                      , userVerkey = Nothing
                                                      , userVerified = True
                                                      }
                                  [ UserName =. name
                                  , UserPassword =. Nothing
                                  , UserAuthType =. UserAuthTypeGoogle
                                  , UserVerkey =. Nothing
                                  , UserVerified =. True
                                  ]
                                  
                  _ <- return $ UserError InvalidLogin

                  case picture of
                    Just src -> do
                        r <- liftIO $ W.get (unpack src)
                        case (r ^? W.responseHeader "Content-Type" . to decodeUtf8, BSL.toStrict <$> r ^? W.responseBody) of
                            (Just mime, Just bs) -> void $ runDB $ upsert
                                UserPhoto { userPhotoUser = uid
                                          , userPhotoMime = mime
                                          , userPhotoPhoto = bs
                                          , userPhotoAttribution = Nothing
                                          }
                                [UserPhotoMime =. mime, UserPhotoPhoto =. bs]
                                
                            _otherwise -> return ()
                            
                    Nothing -> return ()
                  return $ Authenticated uid
                  
              _otherwise -> return $ UserError InvalidLogin

      _ -> do
          user <- runDB $ selectOne $ do
              x <- from $ table @User
              where_ $ x ^. UserEmail E.==. val ident
              return x
          case user of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> return $ UserError InvalidLogin

    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [ authHashDBWithForm formLogin (Just . UniqueUser)
                    ]

    renderAuthMessage :: App -> [Text] -> AuthMessage -> Text
    renderAuthMessage _ [] = defaultMessage
    renderAuthMessage _ ("en":_) = englishMessage
    renderAuthMessage _ ("ru":_) = russianMessage
    renderAuthMessage app (_:xs) = renderAuthMessage app xs

    

formLogin :: Route App -> Widget
formLogin route = do

    users <- liftHandler $ runDB $ select $ from $
        ( do
              x <- from $ table @User
              where_ $ not_ $ x ^. UserSuper
              return x
        )
        `unionAll_`
        ( do
              x <- from $ table @User
              where_ $ x ^. UserSuper
              return x
        )
    
    msgs <- getMessages
    idInputUsername <- newIdent
    idInputPassword <- newIdent
    $(widgetFile "auth/form")


isAuthenticatedSelf :: UserId -> Handler AuthResult
isAuthenticatedSelf uid = do
    muid <- maybeAuthId
    case muid of
        Just uid' | uid == uid' -> return Authorized
                  | otherwise -> unauthorizedI MsgAnotherAccountAccessProhibited
        Nothing -> unauthorizedI MsgLoginPlease


isAdmin :: Handler AuthResult
isAdmin = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ True _ _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ True _ _ _)) -> return Authorized
        Just (Entity _ (User _ _ _ _ False _ _ _)) -> unauthorizedI MsgAccessDeniedAdminsOnly
        Nothing -> unauthorizedI MsgSignInToAccessPlease


isAdministrator :: Handler Bool
isAdministrator = do
    user <- maybeAuth
    case user of
        Just (Entity _ (User _ _ _ _ True _ _ _)) -> return True
        Just (Entity _ (User _ _ _ _ False _ _ _)) -> return False
        Nothing -> return False


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    case muid of
        Nothing -> unauthorizedI MsgLoginToAccessPlease
        Just _ -> return Authorized



instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ [] = defaultFormMessage
    renderMessage _ ("en":_) = englishFormMessage
    renderMessage _ ("ru":_) = russianFormMessage
    renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
