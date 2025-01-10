{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Participants
  ( getParticipantsR, postParticipantsR
  , getParticipantR, postParticipantR
  , getParticipantNewR
  , getParticipantEditR
  , postParticipantDeleR
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
    , DataR (ParticipantNewR, ParticipantR, ParticipantsR, ParticipantEditR, ParticipantDeleR)
    , AppMessage
      ( MsgParticipant, MsgParticipants, MsgNoDataYet, MsgName
      , MsgAlreadyExists, MsgSave, MsgCancel, MsgRecordAdded, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      , MsgDeleteAreYouSure, MsgPhone, MsgEmail
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , ParticipantId, Participant(Participant, participantName, participantPhone, participantEmail)
    , EntityField (ParticipantName, ParticipantId)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , whamlet, SomeMessage (SomeMessage), addMessageI, redirect
    )
import Yesod.Form.Fields (textField)
import Yesod.Form.Functions (mreq, checkM, generateFormPost, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , Field, FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postParticipantDeleR :: ParticipantId -> Handler Html
postParticipantDeleR pid = do
    ((fr,_),_) <- runFormPost formParticipantDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR ParticipantsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR ParticipantsR


postParticipantR :: ParticipantId -> Handler Html
postParticipantR pid = do
    participant <- runDB $ selectOne $ do
        x <- from $ table @Participant
        where_ $ x ^. ParticipantId ==. val pid
        return x
    
    ((fr,fw),et) <- runFormPost $ formParticipant participant
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ParticipantR pid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgParticipant
              idOverlay <- newIdent
              $(widgetFile "data/participants/edit")


getParticipantEditR :: ParticipantId -> Handler Html
getParticipantEditR pid = do
    participant <- runDB $ selectOne $ do
        x <- from $ table @Participant
        where_ $ x ^. ParticipantId ==. val pid
        return x
    
    (fw,et) <- generateFormPost $ formParticipant participant

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParticipant
        idOverlay <- newIdent
        $(widgetFile "data/participants/edit")


getParticipantR :: ParticipantId -> Handler Html
getParticipantR pid = do
    participant <- runDB $ selectOne $ do
        x <- from $ table @Participant
        where_ $ x ^. ParticipantId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formParticipantDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParticipant
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/participants/participant") 


postParticipantsR :: Handler Html
postParticipantsR = do
    ((fr,fw),et) <- runFormPost $ formParticipant Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ParticipantsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgParticipants
              idOverlay <- newIdent
              $(widgetFile "data/participants/new")


getParticipantNewR :: Handler Html
getParticipantNewR = do
    (fw,et) <- generateFormPost $ formParticipant Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParticipants
        idOverlay <- newIdent
        $(widgetFile "data/participants/new")


getParticipantsR :: Handler Html
getParticipantsR = do
    participants <- runDB $ select $ from $ table @Participant

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParticipants
        idOverlay <- newIdent
        $(widgetFile "data/participants/participants")


formParticipant :: Maybe (Entity Participant) -> Form Participant
formParticipant participant extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (participantName . entityVal <$> participant) 

    (phoneR,phoneV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgPhone
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (participantPhone . entityVal <$> participant)

    (emailR,emailV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgEmail
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (participantEmail . entityVal <$> participant)

    return ( Participant <$> nameR <*> phoneR <*> emailR
           , [whamlet|
                     ^{extra}
                     ^{md3widget nameV}
                     ^{md3widget phoneV}
                     ^{md3widget emailV}
                     |]
           )
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Participant
              where_ $ x ^. ParticipantName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity pid _) -> case participant of
              Nothing -> Left MsgAlreadyExists
              Just (Entity pid' _) | pid == pid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formParticipantDelete :: Form ()
formParticipantDelete extra = return (pure (),[whamlet|^{extra}|])
