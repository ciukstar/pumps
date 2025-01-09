{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Docs
  ( getDocsR
  ) where

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (HomeR, StaticR)
    , AppMessage
      ( MsgAppName, MsgDocumentation, MsgIssueTracking, MsgSourceCode
      , MsgOverview, MsgUseCaseDiagram, MsgEntityRelationshipDiagram
      , MsgSuperuser, MsgUsername, MsgPassword, MsgUserRoles
      , MsgDataAdministrator, MsgBasicEntities, MsgParticipant, MsgSurveySheet
      , MsgDoc000, MsgDoc001, MsgDoc002, MsgDoc003, MsgDoc004
      , MsgDoc007
      )
    )

import Settings (widgetFile)
import Settings.StaticFiles
    ( img_Pumps_UCD_svg, img_Pumps_ERD_svg
    )

import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet (Html)

import Yesod
    ( getMessageRender, getUrlRender
    )
import Yesod.Core
    ( Yesod(defaultLayout), newIdent, getMessages
    )
import Yesod.Core.Widget (setTitleI)


getDocsR :: Handler Html
getDocsR = do
    r <- getUrlRender
    m <- getMessageRender
    msgs <- getMessages
    let t = preEscapedToHtml . m
    defaultLayout $ do
        setTitleI MsgDocumentation
        idOverlay <- newIdent
        $(widgetFile "docs/docs")
