{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Risks
  ( getRisksR, postRisksR
  , getRiskR, postRiskR
  , getRiskNewR
  , getRiskEditR
  , postRiskDeleR
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
    , DataR (RiskNewR, RiskR, RisksR, RiskEditR, RiskDeleR)
    , AppMessage
      ( MsgRisk, MsgRisks, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Risk(Risk, riskName), RiskId
    , EntityField (RiskName, RiskId)
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


postRiskDeleR :: RiskId -> Handler Html
postRiskDeleR rid = do
    ((fr,_),_) <- runFormPost formRiskDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete rid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR RisksR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR RisksR


postRiskR :: RiskId -> Handler Html
postRiskR rid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @Risk
        where_ $ x ^. RiskId ==. val rid
        return x
    
    ((fr,fw),et) <- runFormPost $ formRisk typ
    case fr of
      FormSuccess r -> do
          runDB $ replace rid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ RiskR rid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRisk
              idOverlay <- newIdent
              $(widgetFile "data/pump/risks/edit")


getRiskEditR :: RiskId -> Handler Html
getRiskEditR rid = do
    risk <- runDB $ selectOne $ do
        x <- from $ table @Risk
        where_ $ x ^. RiskId ==. val rid
        return x
    
    (fw,et) <- generateFormPost $ formRisk risk

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRisk
        idOverlay <- newIdent
        $(widgetFile "data/pump/risks/edit")


getRiskR :: RiskId -> Handler Html
getRiskR rid = do
    risk <- runDB $ selectOne $ do
        x <- from $ table @Risk
        where_ $ x ^. RiskId ==. val rid
        return x

    (fw0,et0) <- generateFormPost formRiskDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRisk
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/risks/risk")


postRisksR :: Handler Html
postRisksR = do
    ((fr,fw),et) <- runFormPost $ formRisk Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR RisksR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgRisks
              idOverlay <- newIdent
              $(widgetFile "data/pump/risks/new")


getRiskNewR :: Handler Html
getRiskNewR = do
    (fw,et) <- generateFormPost $ formRisk Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRisks
        idOverlay <- newIdent
        $(widgetFile "data/pump/risks/new")


getRisksR :: Handler Html
getRisksR = do
    risks <- runDB $ select $ from $ table @Risk

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgRisks
        idOverlay <- newIdent
        $(widgetFile "data/pump/risks/risks")


formRisk :: Maybe (Entity Risk) -> Form Risk
formRisk typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (riskName . entityVal <$> typ) 

    return ( Risk <$> nameR
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
              x <- from $ table @Risk
              where_ $ x ^. RiskName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formRiskDelete :: Form ()
formRiskDelete extra = return (pure (),[whamlet|^{extra}|])
