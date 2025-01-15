{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Surveys
  ( getSurveysR
  , getSurveyR
  ) where

import Database.Esqueleto.Experimental
    ( select, from, table, selectOne, where_, val, innerJoin, on
    , (^.), (==.), (:&)((:&))
    )
import Database.Persist (Entity(Entity), entityVal)

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (SurveyR, SurveysR)
    , AppMessage
      ( MsgSurveySheet, MsgSurveySheets, MsgNoDataYet, MsgSurveySheets
      , MsgSurveySheet, MsgPumpPositionCode, MsgProcedureNumber, MsgNo
      , MsgQuantity, MsgDateOfFilling, MsgCustomer, MsgRisk, MsgYes
      , MsgProcedureStartDate, MsgCompletionDate, MsgResponsibleCustomer
      , MsgCustomerPhone, MsgCustomerEmail, MsgOfferDate, MsgServicePart
      , MsgResponsibleExecutor, MsgResponsibleFilling, MsgBasicInformation
      , MsgPumpType, MsgPumpOrientation, MsgManufacturingStandard, MsgLocation
      , MsgPumpLayout, MsgPumpClass
      )
    )

import Model
    ( Participant (Participant)
    , PumpType (pumpTypeName)
    , PumpOrientation (pumpOrientationName)
    , PumpClass (pumpClassName)
    , PumpLayout (pumpLayoutName)
    , Standard (standardName)
    , Location (locationName)
    , SheetId
    , Sheet
      ( sheetProcedure, sheetItem, sheetDateFill, sheetRiskSign
      , sheetQuantity, sheetProcedureStartDate, sheetProcedureEndDate
      , sheetResponsibleExecutor, sheetResponsibleFilling, sheetOfferDate
      )
    , EntityField
      ( SheetId, ParticipantId, SheetCustomer, SheetResponsibleCustomer
      , SheetPumpType, PumpTypeId, SheetPumpOrientation, PumpOrientationId
      , SheetPumpLayout, PumpLayoutId, SheetPumpClass, PumpClassId, StandardId
      , SheetStandard, SheetLocation, LocationId
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)
import Text.Julius (rawJS)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getSurveyR :: SheetId -> Handler Html
getSurveyR sid = do
    survey <- runDB $ selectOne $ do
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

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheet
        idOverlay <- newIdent
        classDetails <- newIdent
        $(widgetFile "surveys/survey")


getSurveysR :: Handler Html
getSurveysR = do
    sheets <- runDB $ select $ from $ table @Sheet

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheets 
        idOverlay <- newIdent
        $(widgetFile "surveys/surveys")
