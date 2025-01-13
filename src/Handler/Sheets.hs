{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Sheets
  ( getSheetsR, postSheetsR
  , getSheetR, postSheetR
  , getSheetNewR
  , getSheetEditR
  , postSheetDeleR
  ) where


import Data.Bifunctor (Bifunctor(bimap))
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, Entity (entityVal), selectOne, where_, val
    , (^.), (==.), (:&)((:&))
    , orderBy, asc, Value (unValue), innerJoin, on
    )
import Database.Persist (Entity(Entity), insert_, replace, delete)

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
      , MsgFlanges, MsgBasicInformationAboutThePumpedLiquid, MsgProcedureStartDate, MsgCompletionDate, MsgResponsibleCustomer
      )
    )
    
import Material3 (md3widget, md3switchWidget, md3selectWidget)

import Model
    ( msgSuccess, msgError
    , Participant (Participant)
    , SheetId
    , Sheet
      ( Sheet, sheetProcedure, sheetItem, sheetDateFill, sheetRiskSign
      , sheetQuantity, sheetCustomer, sheetProcedureStartDate, sheetProcedureEndDate, sheetResponsibleCustomer
      )
    , EntityField
      ( SheetId, SheetProcedure, ParticipantName, ParticipantId, SheetCustomer, SheetResponsibleCustomer
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , whamlet, SomeMessage (SomeMessage), addMessageI, MonadHandler (liftHandler)
    , redirect
    )
import Yesod.Form.Fields
    ( textField, dayField, doubleField, checkBoxField, selectField, optionsPairs
    )
import Yesod.Form.Functions (mreq, checkM, generateFormPost, runFormPost)
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
    typ <- runDB $ selectOne $ do
        x <- from $ table @Sheet
        where_ $ x ^. SheetId ==. val sid
        return x
    
    ((fr,fw),et) <- runFormPost $ formSheet typ
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
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
        x <- from $ table @Sheet
        where_ $ x ^. SheetId ==. val sid
        return x
    
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
        x :& c :& r <- from $ table @Sheet
            `innerJoin` table @Participant `on` (\(x :& c) -> x ^. SheetCustomer ==. c ^. ParticipantId)
            `innerJoin` table @Participant `on` (\(x :& _ :& r) -> x ^. SheetResponsibleCustomer ==. r ^. ParticipantId)
        where_ $ x ^. SheetId ==. val sid
        return ((x,c),r)

    (fw0,et0) <- generateFormPost formSheetDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheet
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/sheets/sheet")


postSheetsR :: Handler Html
postSheetsR = do
    ((fr,fw),et) <- runFormPost $ formSheet Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
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


formSheet :: Maybe (Entity Sheet) -> Form Sheet
formSheet sheet extra = do

    customerOptions <- liftHandler $ optionsPairs . (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Participant
        orderBy [asc (x ^. ParticipantName), asc (x ^. ParticipantId)]
        return (x ^. ParticipantName, x ^. ParticipantId) )
    
    (customerR,customerV) <- mreq (selectField customerOptions) FieldSettings
        { fsLabel = SomeMessage MsgCustomer
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetCustomer . entityVal <$> sheet)

    (procedureR,procedureV) <- mreq uniqueProcedureField FieldSettings
        { fsLabel = SomeMessage MsgProcedureNumber
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedure . entityVal <$> sheet)

    (procedureStartDateR,procedureStartDateV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgProcedureStartDate
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedureStartDate . entityVal <$> sheet)

    (procedureEndDateR,procedureEndDateV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgCompletionDate
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetProcedureEndDate . entityVal <$> sheet)
    
    (responsibleCustomerR,responsibleCustomerV) <- mreq (selectField customerOptions) FieldSettings
        { fsLabel = SomeMessage MsgResponsibleCustomer
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetResponsibleCustomer . entityVal <$> sheet)

    (itemR,itemV) <- mreq textField FieldSettings
        { fsLabel = SomeMessage MsgPumpPositionCode
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetItem . entityVal <$> sheet) 

    (dateFillR,dateFillV) <- mreq dayField FieldSettings
        { fsLabel = SomeMessage MsgDateOfFilling
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetDateFill . entityVal <$> sheet)

    (riskSignR,riskSignV) <- mreq checkBoxField FieldSettings
        { fsLabel = SomeMessage MsgRisks
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetRiskSign . entityVal <$> sheet) 

    (quantityR,quantityV) <- mreq doubleField FieldSettings
        { fsLabel = SomeMessage MsgQuantity
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sheetQuantity . entityVal <$> sheet) 

    let r = Sheet <$> customerR <*> responsibleCustomerR
            <*> procedureR <*> procedureStartDateR <*> procedureEndDateR
            <*> itemR <*> dateFillR <*> riskSignR <*> quantityR

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
              Just (Entity sid' _) | sid == sid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formSheetDelete :: Form ()
formSheetDelete extra = return (pure (),[whamlet|^{extra}|])
