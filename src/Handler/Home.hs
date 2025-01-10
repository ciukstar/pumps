{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Handler.Home
  ( getHomeR
  ) where

import Database.Esqueleto.Experimental
    ( SqlExpr, Value (unValue), selectOne, from, table, countRows
    )
    
import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (SurveysR)
    , AppMessage
      ( MsgAppName, MsgWelcome, MsgSurveySheets
      )
    )
    
import Model (Sheet)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages
    , getMessageRender, newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do

    nSheets <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Sheet
        return (countRows :: SqlExpr (Value Int)) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        $(widgetFile "homepage")
