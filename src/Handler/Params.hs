{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Params
  ( getParamsR, postParamsR
  , getParamR, postParamR
  , getParamNewR
  , getParamEditR
  , postParamDeleR
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
    , DataR (ParamNewR, ParamR, ParamsR, ParamEditR, ParamDeleR)
    , AppMessage
      ( MsgParameter, MsgParameters, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Param(Param, paramName), ParamId
    , EntityField (ParamName, ParamId)
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


postParamDeleR :: ParamId -> Handler Html
postParamDeleR pid = do
    ((fr,_),_) <- runFormPost formParamDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete pid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR ParamsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR ParamsR


postParamR :: ParamId -> Handler Html
postParamR pid = do
    param <- runDB $ selectOne $ do
        x <- from $ table @Param
        where_ $ x ^. ParamId ==. val pid
        return x
    
    ((fr,fw),et) <- runFormPost $ formParam param
    case fr of
      FormSuccess r -> do
          runDB $ replace pid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ ParamR pid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgParameter
              idOverlay <- newIdent
              $(widgetFile "data/params/edit")


getParamEditR :: ParamId -> Handler Html
getParamEditR pid = do
    param <- runDB $ selectOne $ do
        x <- from $ table @Param
        where_ $ x ^. ParamId ==. val pid
        return x
    
    (fw,et) <- generateFormPost $ formParam param

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParameter
        idOverlay <- newIdent
        $(widgetFile "data/params/edit")


getParamR :: ParamId -> Handler Html
getParamR pid = do
    param <- runDB $ selectOne $ do
        x <- from $ table @Param
        where_ $ x ^. ParamId ==. val pid
        return x

    (fw0,et0) <- generateFormPost formParamDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParameter
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/params/param")


postParamsR :: Handler Html
postParamsR = do
    ((fr,fw),et) <- runFormPost $ formParam Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR ParamsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgParameters
              idOverlay <- newIdent
              $(widgetFile "data/params/new")


getParamNewR :: Handler Html
getParamNewR = do
    (fw,et) <- generateFormPost $ formParam Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParameters
        idOverlay <- newIdent
        $(widgetFile "data/params/new")


getParamsR :: Handler Html
getParamsR = do
    types <- runDB $ select $ from $ table @Param

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgParameters
        idOverlay <- newIdent
        $(widgetFile "data/params/params")


formParam :: Maybe (Entity Param) -> Form Param
formParam param extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (paramName . entityVal <$> param) 

    return ( Param <$> nameR
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
              x <- from $ table @Param
              where_ $ x ^. ParamName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case param of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formParamDelete :: Form ()
formParamDelete extra = return (pure (),[whamlet|^{extra}|])
