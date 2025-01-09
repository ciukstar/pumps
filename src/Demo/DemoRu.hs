{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoRu (fillDemoRu) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS

import Database.Persist (PersistStoreWrite (insert, insert_))
import Database.Persist.SqlBackend (SqlBackend)

import Model
    ( AuthenticationType (UserAuthTypePassword)
    , User
      ( User, userEmail, userPassword, userSuper, userAdmin, userName
      , userAuthType, userVerkey, userVerified
      )
    , UserPhoto
      ( UserPhoto, userPhotoUser, userPhotoMime, userPhotoAttribution
      , userPhotoPhoto
      )
    , PumpType (PumpType, pumpTypeName)
    , PumpOrientation (PumpOrientation, pumpOrientationName)
    , PumpClass (PumpClass, pumpClassName), PumpLayout (PumpLayout, pumpLayoutName), Standard (Standard, standardName), Location (Location, locationName), Risk (Risk, riskName)
    )
    
import Settings (AppSettings)
    
import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoRu :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoRu _appSettings = do

    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]
    

    pass1 <- liftIO $ saltPass "bulanovalm"
    let user1 = User { userEmail = "bulanovalm@mail.ru"
                     , userPassword = Just pass1
                     , userName = Just "Буланова Любовь Михайловна"
                     , userSuper = False
                     , userAdmin = True
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid1 <- insert user1

    liftIO (BS.readFile "demo/user_1.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid1
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass2 <- liftIO $ saltPass "petrovia"
    let user2 = User { userEmail = "petrovia@mail.ru"
                     , userPassword = Just pass2
                     , userName = Just "Петров Иван Александрович"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid2 <- insert user2

    liftIO (BS.readFile "demo/user_2.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid2
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass3 <- liftIO $ saltPass "smirnovav"
    let user3 = User { userEmail = "smirnovav@mail.ru"
                     , userPassword = Just pass3
                     , userName = Just "Смирнов Андрей Васильевич"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid3 <- insert user3

    liftIO (BS.readFile "demo/user_3.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid3
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    pass4 <- liftIO $ saltPass "sergeevaav"
    let user4 = User { userEmail = "sergeevaav@mail.ru"
                     , userPassword = Just pass4
                     , userName = Just "Сергеева Александра Владимировна"
                     , userSuper = False
                     , userAdmin = False
                     , userAuthType = UserAuthTypePassword
                     , userVerkey = Nothing
                     , userVerified = False
                     }
    uid4 <- insert user4

    liftIO (BS.readFile "demo/user_4.avif") >>= \bs ->
      insert_ UserPhoto { userPhotoUser = uid4
                        , userPhotoMime = "image/avif"
                        , userPhotoPhoto = bs
                        , userPhotoAttribution = Just freepik
                        }

    let ptype1 = PumpType { pumpTypeName = "ЦЕНТРОБЕЖНЫЙ" }
    ptId1 <- insert ptype1

    let ptype2 = PumpType { pumpTypeName = "GEAR" }
    ptId2 <- insert ptype2

    let ptype3 = PumpType { pumpTypeName = "SCREW" }
    ptId3 <- insert ptype3

    let ptype4 = PumpType { pumpTypeName = "HERMETIC" }
    ptId4 <- insert ptype4

    let ptype5 = PumpType { pumpTypeName = "MULTISTAGE" }
    ptId5 <- insert ptype5
    

    let porient1 = PumpOrientation { pumpOrientationName = "Горизонтальный" }
    poId1 <- insert porient1
    

    let porient2 = PumpOrientation { pumpOrientationName = "Вертикальный" }
    poId2 <- insert porient2
    

    let pclass1 = PumpClass { pumpClassName = "OH1" }
    pcId1 <- insert pclass1

    let pclass2 = PumpClass { pumpClassName = "OH2" }
    pcId2 <- insert pclass2

    let pclass3 = PumpClass { pumpClassName = "BB1" }
    pcId3 <- insert pclass3

    let pclass4 = PumpClass { pumpClassName = "BB3" }
    pcId4 <- insert pclass4

    let pclass5 = PumpClass { pumpClassName = "VS" }
    pcId5 <- insert pclass5
    

    let playout1 = PumpLayout { pumpLayoutName = "Только насос" }
    plId1 <- insert playout1

    let playout2 = PumpLayout { pumpLayoutName = "Без муфты" }
    plId2 <- insert playout2

    let playout3 = PumpLayout { pumpLayoutName = "с Муфтой в сборе" }
    plId3 <- insert playout3

    let playout4 = PumpLayout { pumpLayoutName = "Без рамы" }
    plId4 <- insert playout4


    let standard1 = Standard { standardName = "API 610" }
    stId1 <- insert standard1

    let standard2 = Standard { standardName = "API 685" }
    stId2 <- insert standard2

    let standard3 = Standard { standardName = "ISO" }
    stId3 <- insert standard3

    let standard4 = Standard { standardName = "Gost" }
    stId4 <- insert standard4

    let standard5 = Standard { standardName = "OEM" }
    stId5 <- insert standard5

    let standard6 = Standard { standardName = "ГОСТ 32601-2013" }
    stId6 <- insert standard6


    let location1 = Location { locationName = "ПОМЕЩЕНИЕ" }
    locId6 <- insert location1

    let location2 = Location { locationName = "Под навесом" }
    locId2 <- insert location2

    let location3 = Location { locationName = "OUTDOORS" }
    locId3 <- insert location3

    let location4 = Location { locationName = "OTHER" }
    locId4 <- insert location4


    let risk1 = Risk { riskName = "FLAMABLE" }
    riskId1 <- insert risk1

    let risk2 = Risk { riskName = "Общепром" }
    riskId2 <- insert risk2

    let risk3 = Risk { riskName = "С РИСКАМ" }
    riskId3 <- insert risk3

    return ()
