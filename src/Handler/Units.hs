{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Units
  ( getUnitsR, postUnitsR
  , getUnitR, postUnitR
  , getUnitNewR
  , getUnitEditR
  , postUnitDeleR
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
    , DataR (UnitNewR, UnitR, UnitsR, UnitEditR, UnitDeleR)
    , AppMessage
      ( MsgUnitOfMeasurement, MsgUnitsOfMeasurement, MsgNoDataYet, MsgName
      , MsgAlreadyExists, MsgSave, MsgCancel, MsgRecordAdded, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      , MsgDeleteAreYouSure, MsgSymbol
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Unit(Unit, unitName, unitSymbol), UnitId
    , EntityField (UnitName, UnitId)
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


postUnitDeleR :: UnitId -> Handler Html
postUnitDeleR uid = do
    ((fr,_),_) <- runFormPost formUnitDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete uid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR UnitsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR UnitsR


postUnitR :: UnitId -> Handler Html
postUnitR uid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
    
    ((fr,fw),et) <- runFormPost $ formUnit typ
    case fr of
      FormSuccess r -> do
          runDB $ replace uid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ UnitR uid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgUnitOfMeasurement
              idOverlay <- newIdent
              $(widgetFile "data/pump/units/edit")


getUnitEditR :: UnitId -> Handler Html
getUnitEditR uid = do
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x
    
    (fw,et) <- generateFormPost $ formUnit unit

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitOfMeasurement
        idOverlay <- newIdent
        $(widgetFile "data/pump/units/edit")


getUnitR :: UnitId -> Handler Html
getUnitR uid = do
    unit <- runDB $ selectOne $ do
        x <- from $ table @Unit
        where_ $ x ^. UnitId ==. val uid
        return x

    (fw0,et0) <- generateFormPost formUnitDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitOfMeasurement
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/units/unit")


postUnitsR :: Handler Html
postUnitsR = do
    ((fr,fw),et) <- runFormPost $ formUnit Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR UnitsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgUnitsOfMeasurement
              idOverlay <- newIdent
              $(widgetFile "data/pump/units/new")


getUnitNewR :: Handler Html
getUnitNewR = do
    (fw,et) <- generateFormPost $ formUnit Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitsOfMeasurement
        idOverlay <- newIdent
        $(widgetFile "data/pump/units/new")


getUnitsR :: Handler Html
getUnitsR = do
    units <- runDB $ select $ from $ table @Unit

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgUnitsOfMeasurement
        idOverlay <- newIdent
        $(widgetFile "data/pump/units/units")


formUnit :: Maybe (Entity Unit) -> Form Unit
formUnit unit extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (unitName . entityVal <$> unit) 

    (symbolR,symbolV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgSymbol
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (unitSymbol . entityVal <$> unit) 

    return ( Unit <$> nameR <*> symbolR
           , [whamlet|
                     ^{extra}
                     ^{md3widget nameV}
                     ^{md3widget symbolV}
                     |]
           )
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Unit
              where_ $ x ^. UnitName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity uid _) -> case unit of
              Nothing -> Left MsgAlreadyExists
              Just (Entity uid' _) | uid == uid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formUnitDelete :: Form ()
formUnitDelete extra = return (pure (),[whamlet|^{extra}|])
