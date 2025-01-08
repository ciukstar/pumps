{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handler.PumpTypes
  ( getPumpTypesR
  , getPumpTypeR
  , getPumpTypeNewR
  ) where

import Database.Esqueleto.Experimental (select, from, table)
import Database.Persist (Entity(Entity))

import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (PumpTypeNewR, PumpTypeR)
    , AppMessage (MsgPumpType, MsgPumpTypes, MsgNoDataYet)
    )
    
import Model (PumpType(PumpType), PumpTypeId)

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    )
import Yesod.Persist.Core (YesodPersist(runDB))


getPumpTypeR :: PumpTypeId -> Handler Html
getPumpTypeR typ = undefined


getPumpTypeNewR :: Handler Html
getPumpTypeNewR = undefined


getPumpTypesR :: Handler Html
getPumpTypesR = do
    types <- runDB $ select $ from $ table @PumpType

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpTypes
        idOverlay <- newIdent
        $(widgetFile "data/pump/types/types") 
