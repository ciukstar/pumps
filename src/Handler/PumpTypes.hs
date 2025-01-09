{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.PumpTypes
  ( getPumpTypesR, postPumpTypesR
  , getPumpTypeR, postPumpTypeR
  , getPumpTypeNewR
  , getPumpTypeEditR
  , postPumpTypeDeleR
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
    , DataR (PumpTypeNewR, PumpTypeR, PumpTypesR, PumpTypeEditR, PumpTypeDeleR)
    , AppMessage
      ( MsgPumpType, MsgPumpTypes, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess
    , PumpType(PumpType, pumpTypeName), PumpTypeId
    , EntityField (PumpTypeName, PumpTypeId), msgError
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


postPumpTypeDeleR :: PumpTypeId -> Handler Html
postPumpTypeDeleR tid = do
    ((fr,_),_) <- runFormPost formPumpTypeDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete tid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PumpTypesR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR PumpTypesR


postPumpTypeR :: PumpTypeId -> Handler Html
postPumpTypeR tid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpType
        where_ $ x ^. PumpTypeId ==. val tid
        return x
    
    ((fr,fw),et) <- runFormPost $ formPumpType typ
    case fr of
      FormSuccess r -> do
          runDB $ replace tid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PumpTypeR tid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpType
              idOverlay <- newIdent
              $(widgetFile "data/pump/types/edit")


getPumpTypeEditR :: PumpTypeId -> Handler Html
getPumpTypeEditR tid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpType
        where_ $ x ^. PumpTypeId ==. val tid
        return x
    
    (fw,et) <- generateFormPost $ formPumpType typ

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpType
        idOverlay <- newIdent
        $(widgetFile "data/pump/types/edit")


getPumpTypeR :: PumpTypeId -> Handler Html
getPumpTypeR tid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpType
        where_ $ x ^. PumpTypeId ==. val tid
        return x

    (fw0,et0) <- generateFormPost formPumpTypeDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpType
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/types/type")


postPumpTypesR :: Handler Html
postPumpTypesR = do
    ((fr,fw),et) <- runFormPost $ formPumpType Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PumpTypesR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpTypes
              idOverlay <- newIdent
              $(widgetFile "data/pump/types/new")


getPumpTypeNewR :: Handler Html
getPumpTypeNewR = do
    (fw,et) <- generateFormPost $ formPumpType Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpTypes
        idOverlay <- newIdent
        $(widgetFile "data/pump/types/new")


getPumpTypesR :: Handler Html
getPumpTypesR = do
    types <- runDB $ select $ from $ table @PumpType

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpTypes
        idOverlay <- newIdent
        $(widgetFile "data/pump/types/types")


formPumpType :: Maybe (Entity PumpType) -> Form PumpType
formPumpType typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (pumpTypeName . entityVal <$> typ) 

    return ( PumpType <$> nameR
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
              x <- from $ table @PumpType
              where_ $ x ^. PumpTypeName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formPumpTypeDelete :: Form ()
formPumpTypeDelete extra = return (pure (),[whamlet|^{extra}|])
