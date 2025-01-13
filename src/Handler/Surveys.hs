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
import Database.Persist (Entity(Entity))

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (SurveyR, SurveysR)
    , AppMessage
      ( MsgSurveySheet, MsgSurveySheets, MsgNoDataYet, MsgSurveySheets
      , MsgSurveySheet, MsgPumpPositionCode, MsgProcedureNumber, MsgNo
      , MsgQuantity, MsgDateOfFilling, MsgCustomer, MsgRisk, MsgYes
      , MsgProcedureStartDate, MsgCompletionDate, MsgResponsibleCustomer
      )
    )

import Model
    ( Participant (Participant)
    , SheetId
    , Sheet
      ( Sheet
      )
    , EntityField
      ( SheetId, ParticipantId, SheetCustomer, SheetResponsibleCustomer)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getSurveyR :: SheetId -> Handler Html
getSurveyR sid = do
    survey <- runDB $ selectOne $ do
        x :& c :& r <- from $ table @Sheet
            `innerJoin` table @Participant `on` (\(x :& c) -> x ^. SheetCustomer ==. c ^. ParticipantId)
            `innerJoin` table @Participant `on` (\(x :& _ :& r) -> x ^. SheetResponsibleCustomer ==. r ^. ParticipantId)
        where_ $ x ^. SheetId ==. val sid
        return ((x,c),r)

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSurveySheet
        idOverlay <- newIdent
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
