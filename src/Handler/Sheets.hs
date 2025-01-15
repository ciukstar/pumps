{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Sheets
  ( getSheetsR, postSheetsR
  , getSheetR, postSheetR
  , getSheetNewR
  , getSheetEditR
  , postSheetDeleR
  ) where


import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (Text)
import Data.Time.Calendar (Day)

import Database.Esqueleto.Experimental
    ( select, from, table, Entity (entityVal), selectOne, where_, val
    , (^.), (==.), (:&)((:&))
    , orderBy, asc, innerJoin, on, Value (unValue)
    )
import Database.Persist
    ( Entity(Entity), insert_, replace, delete, upsertBy
    , (=.)
    )

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (SheetNewR, SheetR, SheetsR, SheetEditR, SheetDeleR)
    , AppMessage
      ( MsgSurveySheet, MsgSurveySheets, MsgNoDataYet
      , MsgAlreadyExists, MsgSave, MsgCancel, MsgRecordAdded, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      , MsgDeleteAreYouSure, MsgSurveySheets, MsgSurveySheet, MsgQuantity
      , MsgPumpPositionCode, MsgProcedureNumber, MsgDateOfFilling, MsgRisks
      , MsgCustomer, MsgServicePart, MsgBasicInformation, MsgPumpedLiquid
      , MsgTechnicalInformation, MsgDesignFeatures, MsgElectricMotorInfo
      , MsgBasicInformationAboutThePumpedLiquid, MsgProcedureStartDate
      , MsgCompletionDate, MsgResponsibleCustomer, MsgCustomerPhone
      , MsgCustomerEmail, MsgResponsibleExecutor, MsgResponsibleFilling
      , MsgFlanges, MsgOfferDate, MsgRisk, MsgYes, MsgNo, MsgPumpType
      , MsgPumpOrientation, MsgPumpClass, MsgManufacturingStandard
      , MsgPumpLayout, MsgLocation
      )
    )
    
import Material3 (md3widget, md3switchWidget, md3selectWidget)

import Model
    ( msgSuccess, msgError
    , Participant (Participant, participantName, participantPhone, participantEmail)
    , PumpTypeId, PumpType (pumpTypeName)
    , PumpOrientationId, PumpOrientation (pumpOrientationName)
    , PumpClassId, PumpClass (pumpClassName)
    , PumpLayoutId, PumpLayout (pumpLayoutName)
    , StandardId, Standard (standardName)
    , LocationId, Location (locationName)
    , SheetId, Unique (UniqueParticipant)
    , Sheet
      ( Sheet, sheetProcedure, sheetItem, sheetDateFill, sheetRiskSign
      , sheetQuantity, sheetCustomer, sheetProcedureStartDate, sheetProcedureEndDate
      , sheetResponsibleCustomer, sheetResponsibleExecutor, sheetResponsibleFilling
      , sheetOfferDate, sheetPumpType, sheetPumpOrientation, sheetPumpClass
      , sheetPumpLayout, sheetStandard, sheetLocation
      )
    , EntityField
      ( SheetId, SheetProcedure, ParticipantName, ParticipantId, SheetCustomer
      , SheetResponsibleCustomer, ParticipantPhone, ParticipantEmail, PumpTypeName
      , PumpTypeId, PumpOrientationName, PumpOrientationId, SheetPumpType
      , SheetPumpOrientation, PumpClassName, PumpClassId, PumpLayoutId
      , PumpLayoutName, SheetPumpLayout, SheetPumpClass, StandardName, StandardId
      , SheetStandard, LocationId, LocationName, SheetLocation
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Julius (rawJS)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , whamlet, SomeMessage (SomeMessage), addMessageI, MonadHandler (liftHandler)
    , redirect
    )
import Yesod.Form.Fields
    ( textField, dayField, doubleField, checkBoxField
    , emailField, selectFieldList
    )
import Yesod.Form.Functions (mreq, checkM, generateFormPost, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , Field, FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postSheetDeleR :: SheetId -> Handler Html
postSheetDeleR sid = do
    ((fr,_),_) <- runFormPost formSheetDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR SheetsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR SheetsR


postSheetR :: SheetId -> Handler Html
postSheetR sid = do
    sheet <- runDB $ selectOne $ do
        x :& c :& r <- from $ table @Sheet
            `innerJoin` table @Participant `on` (\(x :& c) -> x ^. SheetCustomer ==. c ^. ParticipantId)
            `innerJoin` table @Participant `on` (\(x :& _ :& r) -> x ^. SheetResponsibleCustomer ==. r ^. ParticipantId)
        where_ $ x ^. SheetId ==. val sid
        return (x,(c,r))
    
    ((fr,fw),et) <- runFormPost $ formSheet sheet
    case fr of
      FormSuccess s -> do
          Entity cid _ <- runDB $ upsertBy (UniqueParticipant (surveyCustomerName s))
              Participant { participantName = surveyCustomerName s
                          , participantPhone = surveyCustomerPhone s
                          , participantEmail = surveyCustomerEmail s
                          }
              [ ParticipantPhone =. surveyCustomerPhone s
              , ParticipantEmail =. surveyCustomerEmail s
              ]
          Entity rid _ <- runDB $ upsertBy (UniqueParticipant (surveyRresponsibleCustomer s))
              Participant { participantName = surveyRresponsibleCustomer s
                          , participantPhone = Nothing
                          , participantEmail = Nothing
                          }
              []
          runDB $ replace sid Sheet { sheetCustomer = cid
                                    , sheetResponsibleCustomer = rid
                                    , sheetResponsibleExecutor = surveyResponsibleExecutor s
                                    , sheetResponsibleFilling = surveyResponsibleFilling s
                                    , sheetProcedure = surveyProcedure s
                                    , sheetProcedureStartDate = surveyProcedureStartDate s
                                    , sheetProcedureEndDate = surveyProcedureEndDate s
                                    , sheetOfferDate = surveyOfferDate s
                                    , sheetItem = surveyItem s
                                    , sheetDateFill = surveyDateFill s
                                    , sheetPumpType = surveyPumpType s
                                    , sheetPumpOrientation = surveyPumpOrientation s
                                    , sheetPumpClass = surveyPumpClass s
                                    , sheetPumpLayout = surveyPumpLayout s
                                    , sheetStandard = surveyStandard s
                                    , sheetLocation = surveyLocation s
                                    , sheetRiskSign = surveyRiskSign s
                                    , sheetQuantity = surveyQuantity s
                                    }
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SheetR sid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSurveySheet
              idOverlay <- newIdent
              $(widgetFile "data/sheets/edit")


getSheetEditR :: SheetId -> Handler Html
getSheetEditR sid = do
    sheet <- runDB $ selectOne $ do
        x :& c :& r <- from $ table @Sheet
            `innerJoin` table @Participant `on` (\(x :& c) -> x ^. SheetCustomer ==. c ^. ParticipantId)
            `innerJoin` table @Participant `on` (\(x :& _ :& r) -> x ^. SheetResponsibleCustomer ==. r ^. ParticipantId)
        where_ $ x ^. SheetId ==. val sid
        return (x,(c,r))
    
    (fw,et) <- generateFormPost $ formSheet sheet

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheet
        idOverlay <- newIdent
        $(widgetFile "data/sheets/edit")


getSheetR :: SheetId -> Handler Html
getSheetR sid = do 
    sheet <- runDB $ selectOne $ do
        x :& c :& r :& t :& o :& k :& l :& s :& loc <- from $ table @Sheet
            `innerJoin` table @Participant
                `on` (\(x :& c) -> x ^. SheetCustomer ==. c ^. ParticipantId)
            `innerJoin` table @Participant
                `on` (\(x :& _ :& r) -> x ^. SheetResponsibleCustomer ==. r ^. ParticipantId)
            `innerJoin` table @PumpType
                `on` (\(x :& _ :& _ :& t) -> x ^. SheetPumpType ==. t ^. PumpTypeId)
            `innerJoin` table @PumpOrientation
                `on` (\(x :& _ :& _ :& _ :& o) -> x ^. SheetPumpOrientation ==. o ^. PumpOrientationId)
            `innerJoin` table @PumpClass
                `on` (\(x :& _ :& _ :& _ :& _ :& k) -> x ^. SheetPumpClass ==. k ^. PumpClassId)
            `innerJoin` table @PumpLayout
                `on` (\(x :& _ :& _ :& _ :& _ :& _ :& l) -> x ^. SheetPumpLayout ==. l ^. PumpLayoutId)
            `innerJoin` table @Standard
                `on` (\(x :& _ :& _ :& _ :& _ :& _ :& _ :& s) -> x ^. SheetStandard ==. s ^. StandardId)
            `innerJoin` table @Location
                `on` (\(x :& _ :& _ :& _ :& _ :& _ :& _ :& _ :& loc) -> x ^. SheetLocation ==. loc ^. LocationId)
        where_ $ x ^. SheetId ==. val sid 
        return (x,((c,r),(t,(o,(k,(l,(s,loc)))))))

    (fw0,et0) <- generateFormPost formSheetDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheet
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        idServicePart <- newIdent
        idBasicInformation <- newIdent
        $(widgetFile "data/sheets/sheet")


postSheetsR :: Handler Html
postSheetsR = do
    ((fr,fw),et) <- runFormPost $ formSheet Nothing
    case fr of
      FormSuccess s -> do
          Entity cid _ <- runDB $ upsertBy (UniqueParticipant (surveyCustomerName s))
              Participant { participantName = surveyCustomerName s
                          , participantPhone = surveyCustomerPhone s
                          , participantEmail = surveyCustomerEmail s
                          }
              [ ParticipantPhone =. surveyCustomerPhone s
              , ParticipantEmail =. surveyCustomerEmail s
              ]
          Entity rid _ <- runDB $ upsertBy (UniqueParticipant (surveyRresponsibleCustomer s))
              Participant { participantName = surveyRresponsibleCustomer s
                          , participantPhone = Nothing
                          , participantEmail = Nothing
                          }
              []
          runDB $ insert_ Sheet { sheetCustomer = cid
                                , sheetResponsibleCustomer = rid
                                , sheetResponsibleExecutor = surveyResponsibleExecutor s
                                , sheetResponsibleFilling = surveyResponsibleFilling s
                                , sheetProcedure = surveyProcedure s
                                , sheetProcedureStartDate = surveyProcedureStartDate s
                                , sheetProcedureEndDate = surveyProcedureEndDate s
                                , sheetOfferDate = surveyOfferDate s
                                , sheetItem = surveyItem s
                                , sheetDateFill = surveyDateFill s
                                , sheetPumpType = surveyPumpType s
                                , sheetPumpOrientation = surveyPumpOrientation s
                                , sheetPumpClass = surveyPumpClass s
                                , sheetPumpLayout = surveyPumpLayout s
                                , sheetStandard = surveyStandard s
                                , sheetLocation = surveyLocation s
                                , sheetRiskSign = surveyRiskSign s
                                , sheetQuantity = surveyQuantity s
                                }
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR SheetsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSurveySheet
              idOverlay <- newIdent
              $(widgetFile "data/sheets/new")


getSheetNewR :: Handler Html
getSheetNewR = do
    (fw,et) <- generateFormPost $ formSheet Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheets
        idOverlay <- newIdent
        $(widgetFile "data/sheets/new")


getSheetsR :: Handler Html
getSheetsR = do
    sheets <- runDB $ select $ from $ table @Sheet

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheets
        idOverlay <- newIdent
        $(widgetFile "data/sheets/sheets")

data Survey = Survey
    { surveyCustomerName :: Text
    , surveyCustomerPhone :: Maybe Text
    , surveyCustomerEmail :: Maybe Text
    , surveyRresponsibleCustomer :: Text
    , surveyResponsibleExecutor :: Text
    , surveyResponsibleFilling :: Text
    , surveyProcedure :: Text
    , surveyProcedureStartDate :: Day
    , surveyProcedureEndDate :: Day
    , surveyOfferDate :: Day
    , surveyItem :: Text
    , surveyDateFill :: Day
    , surveyPumpType :: PumpTypeId
    , surveyPumpOrientation :: PumpOrientationId
    , surveyPumpClass :: PumpClassId
    , surveyPumpLayout :: PumpLayoutId
    , surveyStandard :: StandardId
    , surveyLocation :: LocationId
    , surveyRiskSign :: Bool
    , surveyQuantity :: Double
    }

type Customer = Participant

type ResponsibleCustomer = Participant

formSheet :: Maybe (Entity Sheet, (Entity Customer,Entity ResponsibleCustomer))
          -> Form Survey
formSheet sheet extra = do

    classDetails <- newIdent
    idCustomerList <- newIdent

    customers <- liftHandler $ (unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Participant
        orderBy [asc (x ^. ParticipantName), asc (x ^. ParticipantId)]
        return $ x ^. ParticipantName )
    
    (customerNameR,customerNameV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgCustomer
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("list", idCustomerList)]
        } (participantName . entityVal . fst . snd <$> sheet)

    (customerPhoneR,customerPhoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgCustomerPhone
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (participantPhone . entityVal . fst . snd <$> sheet)

    (customerEmailR,customerEmailV) <- mopt emailField FieldSettings
        { fsLabel = SomeMessage MsgCustomerEmail
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (participantEmail . entityVal . fst . snd <$> sheet)

    (responsibleExecutorR,responsibleExecutorV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgResponsibleExecutor
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetResponsibleExecutor . entityVal . fst <$> sheet)

    (fillingRresponsibleR,fillingRresponsibleV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgResponsibleFilling
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetResponsibleFilling . entityVal . fst <$> sheet)

    (procedureR,procedureV) <- mreq uniqueProcedureField FieldSettings
        { fsLabel = SomeMessage MsgProcedureNumber
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedure . entityVal . fst <$> sheet)

    (procedureStartDateR,procedureStartDateV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgProcedureStartDate
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedureStartDate . entityVal . fst <$> sheet)

    (procedureEndDateR,procedureEndDateV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgCompletionDate
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedureEndDate . entityVal . fst <$> sheet)
    
    (responsibleCustomerR,responsibleCustomerV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgResponsibleCustomer
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing
        , fsAttrs = [("list", idCustomerList)]
        } (participantName . entityVal . snd . snd <$> sheet)

    (offerDateR,offerDateV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgOfferDate
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetOfferDate . entityVal . fst <$> sheet)

    (itemR,itemV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgPumpPositionCode
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetItem . entityVal . fst <$> sheet)

    (dateFillR,dateFillV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDateOfFilling
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetDateFill . entityVal . fst <$> sheet)

    pumpTypes <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @PumpType
        orderBy [asc (x ^. PumpTypeName), asc (x ^. PumpTypeId)]
        return (x ^. PumpTypeName, x ^. PumpTypeId) )

    (pumpTypeR,pumpTypeV) <- mreq (selectFieldList pumpTypes) FieldSettings
        { fsLabel = SomeMessage MsgPumpType
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetPumpType . entityVal . fst <$> sheet)

    pumpOrientations <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @PumpOrientation
        orderBy [asc (x ^. PumpOrientationName), asc (x ^. PumpOrientationId)]
        return (x ^. PumpOrientationName, x ^. PumpOrientationId) )

    (pumpOrientationR,pumpOrientationV) <- mreq (selectFieldList pumpOrientations) FieldSettings
        { fsLabel = SomeMessage MsgPumpOrientation
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetPumpOrientation . entityVal . fst <$> sheet)

    pumpClasses <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @PumpClass
        orderBy [asc (x ^. PumpClassName), asc (x ^. PumpClassId)]
        return (x ^. PumpClassName, x ^. PumpClassId) )

    (pumpClassR,pumpClassV) <- mreq (selectFieldList pumpClasses) FieldSettings
        { fsLabel = SomeMessage MsgPumpClass
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetPumpClass . entityVal . fst <$> sheet)

    pumpLayouts <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @PumpLayout
        orderBy [asc (x ^. PumpLayoutName), asc (x ^. PumpLayoutId)]
        return (x ^. PumpLayoutName, x ^. PumpLayoutId) )

    (pumpLayoutR,pumpLayoutV) <- mreq (selectFieldList pumpLayouts) FieldSettings
        { fsLabel = SomeMessage MsgPumpLayout
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetPumpLayout . entityVal . fst <$> sheet)

    pumpStandards <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Standard
        orderBy [asc (x ^. StandardName), asc (x ^. StandardId)]
        return (x ^. StandardName, x ^. StandardId) )

    (standardR,standardV) <- mreq (selectFieldList pumpStandards) FieldSettings
        { fsLabel = SomeMessage MsgManufacturingStandard
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetStandard . entityVal . fst <$> sheet)

    pumpLocations <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Location
        orderBy [asc (x ^. LocationName), asc (x ^. LocationId)]
        return (x ^. LocationName, x ^. LocationId) )

    (locationR,locationV) <- mreq (selectFieldList pumpLocations) FieldSettings
        { fsLabel = SomeMessage MsgLocation
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetLocation . entityVal . fst <$> sheet)

    (riskSignR,riskSignV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgRisks
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetRiskSign . entityVal . fst <$> sheet) 

    (quantityR,quantityV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetQuantity . entityVal . fst <$> sheet)

    let r = Survey <$> customerNameR <*> customerPhoneR <*> customerEmailR
            <*> responsibleCustomerR <*> responsibleExecutorR <*> fillingRresponsibleR
            <*> procedureR <*> procedureStartDateR <*> procedureEndDateR
            <*> offerDateR
            <*> itemR <*> dateFillR
            <*> pumpTypeR
            <*> pumpOrientationR
            <*> pumpClassR
            <*> pumpLayoutR
            <*> standardR
            <*> locationR
            <*> riskSignR <*> quantityR

    return ( r
           , $(widgetFile "data/sheets/form") 
           )
  where
      uniqueProcedureField :: Field Handler Text
      uniqueProcedureField = checkM uniqueProcedure textField

      uniqueProcedure :: Text -> Handler (Either AppMessage Text)
      uniqueProcedure name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Sheet
              where_ $ x ^. SheetProcedure ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity sid _) -> case sheet of
              Nothing -> Left MsgAlreadyExists
              Just (Entity sid' _,_) | sid == sid' -> Right name
                                     | otherwise -> Left MsgAlreadyExists


formSheetDelete :: Form ()
formSheetDelete extra = return (pure (),[whamlet|^{extra}|])
