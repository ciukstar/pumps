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


import Foundation
    ( Handler, widgetSnackbar, widgetTopbar
    , AppMessage
      ( MsgAppName, MsgWelcomeTo
      )
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), getMessages
    , getMessageRender, newIdent
    )
import Yesod.Core.Widget (setTitleI)


getHomeR :: Handler Html
getHomeR = do
    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgAppName

        idOverlay <- newIdent
        idMain <- newIdent
        $(widgetFile "homepage")
