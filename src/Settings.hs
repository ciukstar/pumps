{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TypeOperators     #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson
    ( Result (..), fromJSON, withObject, (.!=), (.:?) )
import Data.Aeson.Types (Parser)
import Data.FileEmbed (embedFile)
import Data.Time.LocalTime (TimeZone, utc)
import Data.Yaml (decodeEither')

import Database.Persist.Sqlite
    ( SqliteConf
    , ConnectionPoolConfig
      ( ConnectionPoolConfig, connectionPoolConfigStripes
      , connectionPoolConfigIdleTimeout, connectionPoolConfigSize
      )
    )

import Text.Read (readMaybe)
    
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)


data Superuser = Superuser { superuserUsername :: Text
                           , superuserPassword :: Text
                           }


data GoogleApiConf = GoogleApiConf { googleApiConfClientId :: Text
                                   , googleApiConfClientSecret :: Text
                                   }

newtype GcloudConf = GcloudConf { gcloudProjectId :: Text }


-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , appConnectionPoolConfig   :: ConnectionPoolConfig
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDevelopment            :: Bool
    -- ^ Development mode
    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code
    
    , appTimeZone    :: TimeZone
    -- ^ Time Zone
    , appSuperuser              :: Superuser
    , appGoogleApiConf          :: GoogleApiConf
    , appGcloudConf             :: GcloudConf
    -- ^ Google API config

    , appAuthDummyLogin         :: Bool
    -- ^ Indicate if auth dummy login should be enabled.
    }

instance FromJSON Superuser where
    parseJSON :: Value -> Parser Superuser
    parseJSON = withObject "Superuser" $ \o -> do
        superuserUsername <- o .: "username"
        superuserPassword <- o .: "password"
        return Superuser {..}


instance FromJSON GoogleApiConf where
    parseJSON :: Value -> Parser GoogleApiConf
    parseJSON = withObject "GoogleApiConf" $ \o -> do
        googleApiConfClientId     <- o .: "client-id"
        googleApiConfClientSecret <- o .: "client-secret"
        return GoogleApiConf {..}


instance FromJSON GcloudConf where
    parseJSON :: Value -> Parser GcloudConf
    parseJSON = withObject "GcloudConf" $ \o -> do
        gcloudProjectId <- o .: "project-id"
        return GcloudConf {..}


instance FromJSON ConnectionPoolConfig where
    parseJSON :: Value -> Parser ConnectionPoolConfig
    parseJSON = withObject "ConnectionPoolConfig" $ \o -> do
        connectionPoolConfigStripes     <- o .: "stripes"
        connectionPoolConfigIdleTimeout <- o .: "idle-timeout"
        connectionPoolConfigSize        <- o .: "size"
        return ConnectionPoolConfig {..}


instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appConnectionPoolConfig   <- o .: "connection-pool"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDevelopment            <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"
                                     
        appTimeZone  <- fromMaybe utc . readMaybe <$> o .: "time-zone"

        appSuperuser     <- o .:  "superuser"
        appGoogleApiConf <- o .: "google-api"
        appGcloudConf    <- o .: "gcloud"

        appAuthDummyLogin <- o .:? "auth-dummy-login"      .!= dev

        return AppSettings {..}

      


-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
