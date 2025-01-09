{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.PumpOrientations
  ( getPumpOrientationsR, postPumpOrientationsR
  , getPumpOrientationR, postPumpOrientationR
  , getPumpOrientationNewR
  , getPumpOrientationEditR
  , postPumpOrientationDeleR
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
    , DataR
      ( PumpOrientationNewR, PumpOrientationR, PumpOrientationsR, PumpOrientationEditR
      , PumpOrientationDeleR
      )
    , AppMessage
      ( MsgPumpOrientation, MsgPumpOrientations, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , PumpOrientation(PumpOrientation, pumpOrientationName), PumpOrientationId
    , EntityField (PumpOrientationName, PumpOrientationId)
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


postPumpOrientationDeleR :: PumpOrientationId -> Handler Html
postPumpOrientationDeleR oid = do
    ((fr,_),_) <- runFormPost formPumpOrientationDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete oid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PumpOrientationsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR PumpOrientationsR


postPumpOrientationR :: PumpOrientationId -> Handler Html
postPumpOrientationR oid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpOrientation
        where_ $ x ^. PumpOrientationId ==. val oid
        return x
    
    ((fr,fw),et) <- runFormPost $ formPumpOrientation typ
    case fr of
      FormSuccess r -> do
          runDB $ replace oid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PumpOrientationR oid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpOrientation
              idOverlay <- newIdent
              $(widgetFile "data/pump/orientations/edit")


getPumpOrientationEditR :: PumpOrientationId -> Handler Html
getPumpOrientationEditR oid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpOrientation
        where_ $ x ^. PumpOrientationId ==. val oid
        return x
    
    (fw,et) <- generateFormPost $ formPumpOrientation typ

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpOrientation
        idOverlay <- newIdent
        $(widgetFile "data/pump/orientations/edit")


getPumpOrientationR :: PumpOrientationId -> Handler Html
getPumpOrientationR oid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @PumpOrientation
        where_ $ x ^. PumpOrientationId ==. val oid
        return x

    (fw0,et0) <- generateFormPost formPumpOrientationDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpOrientation
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/orientations/orientation")


postPumpOrientationsR :: Handler Html
postPumpOrientationsR = do
    ((fr,fw),et) <- runFormPost $ formPumpOrientation Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PumpOrientationsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpOrientations
              idOverlay <- newIdent
              $(widgetFile "data/pump/orientations/new")


getPumpOrientationNewR :: Handler Html
getPumpOrientationNewR = do
    (fw,et) <- generateFormPost $ formPumpOrientation Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpOrientations
        idOverlay <- newIdent
        $(widgetFile "data/pump/orientations/new")


getPumpOrientationsR :: Handler Html
getPumpOrientationsR = do
    orientations <- runDB $ select $ from $ table @PumpOrientation

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpOrientations
        idOverlay <- newIdent
        $(widgetFile "data/pump/orientations/orientations")


formPumpOrientation :: Maybe (Entity PumpOrientation) -> Form PumpOrientation
formPumpOrientation typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (pumpOrientationName . entityVal <$> typ) 

    return ( PumpOrientation <$> nameR
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
              x <- from $ table @PumpOrientation
              where_ $ x ^. PumpOrientationName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formPumpOrientationDelete :: Form ()
formPumpOrientationDelete extra = return (pure (),[whamlet|^{extra}|])
