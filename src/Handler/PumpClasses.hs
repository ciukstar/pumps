{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.PumpClasses
  ( getPumpClassesR, postPumpClassesR
  , getPumpClassR, postPumpClassR
  , getPumpClassNewR
  , getPumpClassEditR
  , postPumpClassDeleR
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
    , DataR (PumpClassNewR, PumpClassR, PumpClassesR, PumpClassEditR, PumpClassDeleR)
    , AppMessage
      ( MsgPumpClass, MsgPumpClasses, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , PumpClass(PumpClass, pumpClassName), PumpClassId
    , EntityField (PumpClassName, PumpClassId) 
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


postPumpClassDeleR :: PumpClassId -> Handler Html
postPumpClassDeleR cid = do
    ((fr,_),_) <- runFormPost formPumpClassDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete cid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR PumpClassesR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR PumpClassesR


postPumpClassR :: PumpClassId -> Handler Html
postPumpClassR cid = do
    clazz <- runDB $ selectOne $ do
        x <- from $ table @PumpClass
        where_ $ x ^. PumpClassId ==. val cid
        return x
    
    ((fr,fw),et) <- runFormPost $ formPumpClass clazz
    case fr of
      FormSuccess r -> do
          runDB $ replace cid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ PumpClassR cid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpClass
              idOverlay <- newIdent
              $(widgetFile "data/pump/classes/edit")


getPumpClassEditR :: PumpClassId -> Handler Html
getPumpClassEditR cid = do
    clazz <- runDB $ selectOne $ do
        x <- from $ table @PumpClass
        where_ $ x ^. PumpClassId ==. val cid
        return x
    
    (fw,et) <- generateFormPost $ formPumpClass clazz

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpClass
        idOverlay <- newIdent
        $(widgetFile "data/pump/classes/edit")


getPumpClassR :: PumpClassId -> Handler Html
getPumpClassR cid = do
    clazz <- runDB $ selectOne $ do
        x <- from $ table @PumpClass
        where_ $ x ^. PumpClassId ==. val cid
        return x

    (fw0,et0) <- generateFormPost formPumpClassDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpClass
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/classes/class")


postPumpClassesR :: Handler Html
postPumpClassesR = do
    ((fr,fw),et) <- runFormPost $ formPumpClass Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR PumpClassesR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgPumpClasses
              idOverlay <- newIdent
              $(widgetFile "data/pump/classes/new")


getPumpClassNewR :: Handler Html
getPumpClassNewR = do
    (fw,et) <- generateFormPost $ formPumpClass Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpClasses
        idOverlay <- newIdent
        $(widgetFile "data/pump/classes/new")


getPumpClassesR :: Handler Html
getPumpClassesR = do
    classes <- runDB $ select $ from $ table @PumpClass

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgPumpClasses
        idOverlay <- newIdent
        $(widgetFile "data/pump/classes/classes")


formPumpClass :: Maybe (Entity PumpClass) -> Form PumpClass
formPumpClass typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (pumpClassName . entityVal <$> typ) 

    return ( PumpClass <$> nameR
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
              x <- from $ table @PumpClass
              where_ $ x ^. PumpClassName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formPumpClassDelete :: Form ()
formPumpClassDelete extra = return (pure (),[whamlet|^{extra}|])
