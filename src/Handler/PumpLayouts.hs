{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.PumpLayouts
  ( getPumpLayoutsR, postPumpLayoutsR
  , getPumpLayoutR, postPumpLayoutR
  , getPumpLayoutNewR
  , getPumpLayoutEditR
  , postPumpLayoutDeleR
  ) where


import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, Entity (entityVal), selectOne, where_, val
    , (^.), (==.)
    )
import Database.Persist (Entity(Entity), insert_, replace, delete)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (PumpLayoutNewR, PumpLayoutR, PumpLayoutsR, PumpLayoutEditR, PumpLayoutDeleR)
    , AppMessage
      ( MsgPumpLayout, MsgPumpLayouts, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , PumpLayout(PumpLayout, pumpLayoutName), PumpLayoutId
    , EntityField (PumpLayoutName, PumpLayoutId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , whamlet, SomeMessage (SomeMessage), addMessageI, redirect
    )
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (mreq, checkM, generateFormPost, runFormPost)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , Field, FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postPumpLayoutDeleR :: PumpLayoutId -> Handler Html
postPumpLayoutDeleR lid = do
    ((fr,_),_) <- runFormPost formPumpLayoutDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete lid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PumpLayoutsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR PumpLayoutsR


postPumpLayoutR :: PumpLayoutId -> Handler Html
postPumpLayoutR lid = do
    layout <- runDB $ selectOne $ do
        x <- from $ table @PumpLayout
        where_ $ x ^. PumpLayoutId ==. val lid
        return x
    
    ((fr,fw),et) <- runFormPost $ formPumpLayout layout
    case fr of
      FormSuccess r -> do
          runDB $ replace lid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PumpLayoutR lid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpLayout
              idOverlay <- newIdent
              $(widgetFile "data/pump/layouts/edit")


getPumpLayoutEditR :: PumpLayoutId -> Handler Html
getPumpLayoutEditR lid = do
    layout <- runDB $ selectOne $ do
        x <- from $ table @PumpLayout
        where_ $ x ^. PumpLayoutId ==. val lid
        return x
    
    (fw,et) <- generateFormPost $ formPumpLayout layout

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpLayout
        idOverlay <- newIdent
        $(widgetFile "data/pump/layouts/edit")


getPumpLayoutR :: PumpLayoutId -> Handler Html
getPumpLayoutR lid = do
    layout <- runDB $ selectOne $ do
        x <- from $ table @PumpLayout
        where_ $ x ^. PumpLayoutId ==. val lid
        return x

    (fw0,et0) <- generateFormPost formPumpLayoutDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpLayout
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/layouts/layout")


postPumpLayoutsR :: Handler Html
postPumpLayoutsR = do
    ((fr,fw),et) <- runFormPost $ formPumpLayout Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PumpLayoutsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpLayouts
              idOverlay <- newIdent
              $(widgetFile "data/pump/layouts/new")


getPumpLayoutNewR :: Handler Html
getPumpLayoutNewR = do
    (fw,et) <- generateFormPost $ formPumpLayout Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpLayouts
        idOverlay <- newIdent
        $(widgetFile "data/pump/layouts/new")


getPumpLayoutsR :: Handler Html
getPumpLayoutsR = do
    types <- runDB $ select $ from $ table @PumpLayout

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpLayouts
        idOverlay <- newIdent
        $(widgetFile "data/pump/layouts/layouts")


formPumpLayout :: Maybe (Entity PumpLayout) -> Form PumpLayout
formPumpLayout layout extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (pumpLayoutName . entityVal <$> layout) 

    return ( PumpLayout <$> nameR
           , [whamlet|
                     ^{extra}
                     ^{md3widget nameV}
                     |]
           )
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @PumpLayout
              where_ $ x ^. PumpLayoutName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case layout of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formPumpLayoutDelete :: Form ()
formPumpLayoutDelete extra = return (pure (),[whamlet|^{extra}|])
