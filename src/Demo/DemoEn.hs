{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Demo.DemoEn (fillDemoEn) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString as BS
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))

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
    , PumpClass (PumpClass, pumpClassName)
    , PumpLayout (PumpLayout, pumpLayoutName)
    , Standard (Standard, standardName)
    , Location (Location, locationName)
    , Risk (Risk, riskName)
    , Unit (Unit, unitName, unitSymbol)
    , Section (Section, sectionName, sectionParent)
    , Participant (Participant, participantName, participantPhone, participantEmail)
    , Sheet
      ( Sheet, sheetCustomer, sheetProcedure, sheetItem, sheetDateFill, sheetRiskSign
      , sheetQuantity, sheetProcedureStartDate, sheetProcedureEndDate, sheetOfferDate
      , sheetResponsibleCustomer, sheetResponsibleExecutor, sheetResponsibleFilling
      , sheetPumpType, sheetPumpOrientation, sheetPumpClass, sheetPumpLayout
      , sheetStandard, sheetLocation
      ), Param (Param, paramName)
    )
    
import Settings (AppSettings)

import Text.Hamlet (shamlet)

import Yesod.Auth.Email (saltPass)


fillDemoEn :: MonadIO m => AppSettings -> ReaderT SqlBackend m ()
fillDemoEn _appSettings = do

    now <- utctDay <$> liftIO getCurrentTime
    
    let freepik = [shamlet|
                          Designed by #
                          <a href="https://www.freepik.com/" target=_blank>
                            Freepik
                          |]

    pass1 <- liftIO $ saltPass "marylopez"
    let user1 = User { userEmail = "marylopez@xmail.edu"
                     , userPassword = Just pass1
                     , userName = Just "Mary Lopez"
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

    pass2 <- liftIO $ saltPass "jjohnson"
    let user2 = User { userEmail = "jjohnson@xmail.edu"
                     , userPassword = Just pass2
                     , userName = Just "John Johnson"
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

    pass3 <- liftIO $ saltPass "jmaulsby"
    let user3 = User { userEmail = "jmaulsby@xmail.edu"
                     , userPassword = Just pass3
                     , userName = Just "Julian Maulsby"
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

    pass4 <- liftIO $ saltPass "vschoen"
    let user4 = User { userEmail = "vschoen@xmail.edu"
                     , userPassword = Just pass4
                     , userName = Just "Valentina Schoen"
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

    let ptype1 = PumpType { pumpTypeName = "Centrifugal" }
    ptId1 <- insert ptype1

    let ptype2 = PumpType { pumpTypeName = "Gear" }
    ptId2 <- insert ptype2

    let ptype3 = PumpType { pumpTypeName = "Screw" }
    ptId3 <- insert ptype3

    let ptype4 = PumpType { pumpTypeName = "Hermetic" }
    ptId4 <- insert ptype4

    let ptype5 = PumpType { pumpTypeName = "Multistage" }
    ptId5 <- insert ptype5
    

    let porient1 = PumpOrientation { pumpOrientationName = "Horizontal" }
    poId1 <- insert porient1    

    let porient2 = PumpOrientation { pumpOrientationName = "Vertical" }
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
    

    let playout1 = PumpLayout { pumpLayoutName = "Pump only" }
    plId1 <- insert playout1

    let playout2 = PumpLayout { pumpLayoutName = "Without coupling" }
    plId2 <- insert playout2

    let playout3 = PumpLayout { pumpLayoutName = "With coupling assembly" }
    plId3 <- insert playout3

    let playout4 = PumpLayout { pumpLayoutName = "Without frame" }
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

    let standard6 = Standard { standardName = "GOST 32601-2013" }
    stId6 <- insert standard6


    let location1 = Location { locationName = "Indoor" }
    locId1 <- insert location1

    let location2 = Location { locationName = "Under the canopy" }
    locId2 <- insert location2

    let location3 = Location { locationName = "Outdoors" }
    locId3 <- insert location3

    let location4 = Location { locationName = "Other" }
    locId4 <- insert location4


    let risk1 = Risk { riskName = "Flammable" }
    riskId1 <- insert risk1

    let risk2 = Risk { riskName = "General industrial" }
    riskId2 <- insert risk2

    let risk3 = Risk { riskName = "With risks" }
    riskId3 <- insert risk3
    

    let unit1 = Unit { unitName = "Degree Celsius"
                     , unitSymbol = "°C"
                     }
    uId1 <- insert unit1

    let unit2 = Unit { unitName = "Millimetre"
                     , unitSymbol = "mm"
                     }
    uId2 <- insert unit2

    let unit3 = Unit { unitName = "Percent"
                     , unitSymbol = "%"
                     }
    uId3 <- insert unit3

    let unit4 = Unit { unitName = "Millipascal second"
                     , unitSymbol = "mPa·s"
                     }
    uId4 <- insert unit4

    let unit5 = Unit { unitName = "Centipoise"
                     , unitSymbol = "cP"
                     }
    uId5 <- insert unit5

    let unit6 = Unit { unitName = "Saybolt Seconds Universal"
                     , unitSymbol = "SSU"
                     }
    uId6 <- insert unit6

    let unit7 = Unit { unitName = "Centipoise/Millipascal second"
                     , unitSymbol = "cP/mPa·s"
                     }
    uId7 <- insert unit7

    let unit8 = Unit { unitName = "Density SI"
                     , unitSymbol = "kg / m3"
                     }
    uId8 <- insert unit8

    let unit9 = Unit { unitName = "Density 2"
                     , unitSymbol = "kg/l"
                     }
    uId9 <- insert unit9

    let unit10 = Unit { unitName = "Density 3"
                     , unitSymbol = "g/cm3"
                     }
    uId10 <- insert unit10

    
    let participant1 = Participant { participantName = "Syncobur"
                                   , participantPhone = Just "+1098743334"
                                   , participantEmail = Just "syncobur@mail.xyz"
                                   }
    pId1 <- insert participant1
    
    let participant2 = Participant { participantName = "OCHKA"
                                   , participantPhone = Just "+1098743833"
                                   , participantEmail = Just "ochka@mail.xyz"
                                   }
    pId2 <- insert participant2
    
    let participant3 = Participant { participantName = "Cukloin"
                                   , participantPhone = Just "+10987438755"
                                   , participantEmail = Just "cukloin@mail.xyz"
                                   }
    pId3 <- insert participant3
    
    let participant4 = Participant { participantName = "Goloolimer"
                                   , participantPhone = Just "+1098743321"
                                   , participantEmail = Just "goloolimer@mail.xyz"
                                   }
    pId4 <- insert participant4
    
    let participant5 = Participant { participantName = "Jill A. Turnbow"
                                   , participantPhone = Just "+1098743321"
                                   , participantEmail = Just "jaturnbow@mail.xyz"
                                   }
    pId5 <- insert participant5
    
    let participant6 = Participant { participantName = "Charles S. Watkins"
                                   , participantPhone = Just "+188996645"
                                   , participantEmail = Just "cswatkins@xmail.edu"
                                   }
    pId6 <- insert participant6
    
    let participant7 = Participant { participantName = "Donald L. Meyer"
                                   , participantPhone = Just "+18899019282"
                                   , participantEmail = Just "dlmeyer@xmail.edu"
                                   }
    pId7 <- insert participant7
    
    let participant8 = Participant { participantName = "Jean J. Bullock"
                                   , participantPhone = Just "+1889901932132"
                                   , participantEmail = Just "jjbullock@xmail.edu"
                                   }
    pId8 <- insert participant8

    let section1 = Section { sectionName = "Basic Information"
                           , sectionParent = Nothing
                           }
    secId1 <- insert section1

    let section2 = Section { sectionName = "Basic information about the pumped liquid"
                           , sectionParent = Nothing
                           }
    secId2 <- insert section2

    let section3 = Section { sectionName = "Technical information about the pump"
                           , sectionParent = Nothing
                           }
    secId3 <- insert section3

    let section4 = Section { sectionName = "Design features of the pump"
                           , sectionParent = Nothing
                           }
    secId4 <- insert section4

    let section5 = Section { sectionName = "Flanges"
                           , sectionParent = Nothing
                           }
    secId5 <- insert section5

    let section6 = Section { sectionName = "Basic information about the electric motor"
                           , sectionParent = Nothing
                           }
    secId6 <- insert section6

    let param1 = Param { paramName = "Max air temperature (˚C)"
                       }
    prId1 <- insert param1

    let param2 = Param { paramName = "Min air temperature (˚C)"
                       }
    prId2 <- insert param2
    
    let sheet1 = Sheet { sheetCustomer = pId1
                       , sheetResponsibleCustomer = pId5
                       , sheetResponsibleExecutor = participantName participant7
                       , sheetResponsibleFilling = participantName participant8
                       , sheetProcedure = "PROC-0001"
                       , sheetProcedureStartDate = addDays 1 now
                       , sheetProcedureEndDate = addDays 10 now
                       , sheetOfferDate = addDays 2 now
                       , sheetItem = "SA-01"
                       , sheetDateFill = now
                       , sheetPumpType = ptId1
                       , sheetPumpOrientation = poId1
                       , sheetPumpClass = pcId1
                       , sheetPumpLayout = plId1
                       , sheetStandard = stId1
                       , sheetLocation = locId1
                       , sheetRiskSign = True
                       , sheetQuantity = 1
                       }
    sid1 <- insert sheet1
    
    let sheet2 = Sheet { sheetCustomer = pId2
                       , sheetResponsibleCustomer = pId6
                       , sheetResponsibleExecutor = participantName participant7
                       , sheetResponsibleFilling = participantName participant8
                       , sheetProcedure = "PROC-0002"
                       , sheetItem = "SA-01"
                       , sheetDateFill = now
                       , sheetProcedureStartDate = addDays 2 now
                       , sheetOfferDate = addDays 3 now
                       , sheetProcedureEndDate = addDays 10 now
                       , sheetPumpType = ptId2
                       , sheetPumpOrientation = poId2
                       , sheetPumpClass = pcId2
                       , sheetPumpLayout = plId2
                       , sheetStandard = stId2
                       , sheetLocation = locId2
                       , sheetRiskSign = False
                       , sheetQuantity = 2
                       }
    sid2 <- insert sheet2

    
    
    return ()
