{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Standards
  ( getStandardsR, postStandardsR
  , getStandardR, postStandardR
  , getStandardNewR
  , getStandardEditR
  , postStandardDeleR
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
    , DataR (StandardNewR, StandardR, StandardsR, StandardEditR, StandardDeleR)
    , AppMessage
      ( MsgManufacturingStandard, MsgManufacturingStandards, MsgNoDataYet, MsgName
      , MsgAlreadyExists, MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      , MsgDele
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Standard(Standard, standardName), StandardId
    , EntityField (StandardName, StandardId)
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


postStandardDeleR :: StandardId -> Handler Html
postStandardDeleR sid = do
    ((fr,_),_) <- runFormPost formStandardDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR StandardsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR StandardsR


postStandardR :: StandardId -> Handler Html
postStandardR sid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @Standard
        where_ $ x ^. StandardId ==. val sid
        return x
    
    ((fr,fw),et) <- runFormPost $ formStandard typ
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ StandardR sid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgManufacturingStandard
              idOverlay <- newIdent
              $(widgetFile "data/pump/standards/edit")


getStandardEditR :: StandardId -> Handler Html
getStandardEditR sid = do
    standard <- runDB $ selectOne $ do
        x <- from $ table @Standard
        where_ $ x ^. StandardId ==. val sid
        return x
    
    (fw,et) <- generateFormPost $ formStandard standard

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgManufacturingStandard
        idOverlay <- newIdent
        $(widgetFile "data/pump/standards/edit")


getStandardR :: StandardId -> Handler Html
getStandardR sid = do
    standard <- runDB $ selectOne $ do
        x <- from $ table @Standard
        where_ $ x ^. StandardId ==. val sid
        return x

    (fw0,et0) <- generateFormPost formStandardDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgManufacturingStandard
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/standards/standard")


postStandardsR :: Handler Html
postStandardsR = do
    ((fr,fw),et) <- runFormPost $ formStandard Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR StandardsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgManufacturingStandards
              idOverlay <- newIdent
              $(widgetFile "data/pump/standards/new")


getStandardNewR :: Handler Html
getStandardNewR = do
    (fw,et) <- generateFormPost $ formStandard Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgManufacturingStandards
        idOverlay <- newIdent
        $(widgetFile "data/pump/standards/new")


getStandardsR :: Handler Html
getStandardsR = do
    types <- runDB $ select $ from $ table @Standard

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgManufacturingStandards
        idOverlay <- newIdent
        $(widgetFile "data/pump/standards/standards")


formStandard :: Maybe (Entity Standard) -> Form Standard
formStandard typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (standardName . entityVal <$> typ) 

    return ( Standard <$> nameR
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
              x <- from $ table @Standard
              where_ $ x ^. StandardName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formStandardDelete :: Form ()
formStandardDelete extra = return (pure (),[whamlet|^{extra}|])
