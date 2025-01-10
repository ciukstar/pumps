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

import Data.Maybe (maybe) 

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (SurveysR)
    , AppMessage
      ( MsgAppName, MsgWelcome, MsgSurveySheets
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages
    , getMessageRender, newIdent
    )
import Yesod.Core.Widget (setTitleI)
import Model (Sheet(Sheet))
import Database.Esqueleto.Experimental (SqlExpr, selectOne, from, table, countRows, Value (Value, unValue))
import Yesod.Persist.Core (YesodPersist(runDB))


getHomeR :: Handler Html
getHomeR = do

    sheets <- maybe 0 unValue <$> runDB ( selectOne $ do
        _ <- from $ table @Sheet
        return (countRows :: SqlExpr (Value Int)) )
    
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        $(widgetFile "homepage")
