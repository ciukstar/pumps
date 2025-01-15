{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Sections
  ( getSectionsR, postSectionsR
  , getSectionR, postSectionR
  , getSectionNewR
  , getSectionEditR
  , postSectionDeleR
  ) where

import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.List.Safe as LS (last)
import Data.Text (Text)

import Database.Esqueleto.Experimental
    ( select, from, table, Entity (entityVal), selectOne, where_, val
    , (^.), (?.), (==.), (:&)((:&))
    , orderBy, asc, Value (unValue), leftJoin, on, isNothing_
    )
import Database.Persist (Entity(Entity), insert_, replace, delete)

import Foundation
    ( Handler, Form, widgetSnackbar, widgetTopbar
    , Route (DataR)
    , DataR (SectionNewR, SectionR, SectionsR, SectionEditR, SectionDeleR)
    , AppMessage
      ( MsgSection, MsgSections, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgParentSection
      , MsgInvalidFormData, MsgSubsections, MsgDescription
      )
    )
    
import Material3 (md3widget, md3selectWidget)

import Model
    ( msgSuccess, msgError
    , Sections (Sections)
    , SectionId, Section(Section, sectionName, sectionParent)
    , EntityField (SectionName, SectionId, SectionParent)
    )

import Settings (widgetFile)

import Text.Hamlet (Html)

import Yesod.Core
    ( Yesod(defaultLayout), setTitleI, newIdent, getMessages, getMessageRender
    , whamlet, SomeMessage (SomeMessage), addMessageI, redirect
    , MonadHandler (liftHandler)
    )
import Yesod.Form.Fields (textField, selectFieldList)
import Yesod.Form.Functions (mreq, checkM, generateFormPost, runFormPost, mopt)
import Yesod.Form.Types
    ( FieldSettings(FieldSettings, fsLabel, fsId, fsName, fsTooltip, fsAttrs)
    , Field, FormResult (FormSuccess)
    )
import Yesod.Persist.Core (YesodPersist(runDB))


postSectionDeleR :: SectionId -> Sections -> Handler Html
postSectionDeleR sid ps = do
    ((fr,_),_) <- runFormPost formSectionDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete sid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR $ SectionsR ps
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR $ SectionR sid ps


postSectionR :: SectionId -> Sections -> Handler Html
postSectionR sid ps = do
    section <- runDB $ selectOne $ do
        x <- from $ table @Section
        where_ $ x ^. SectionId ==. val sid
        return x
    
    ((fr,fw),et) <- runFormPost $ formSection section
    case fr of
      FormSuccess r -> do
          runDB $ replace sid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ SectionR sid ps
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSection
              idOverlay <- newIdent
              $(widgetFile "data/sections/edit")


getSectionEditR :: SectionId -> Sections -> Handler Html
getSectionEditR sid ps = do
    section <- runDB $ selectOne $ do
        x <- from $ table @Section
        where_ $ x ^. SectionId ==. val sid
        return x
    
    (fw,et) <- generateFormPost $ formSection section

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSection
        idOverlay <- newIdent
        $(widgetFile "data/sections/edit")


getSectionR :: SectionId -> Sections -> Handler Html
getSectionR sid ps@(Sections ss) = do
    section <- runDB $ selectOne $ do
        x :& p <- from $ table @Section
            `leftJoin` table @Section `on` (\(x :& p) -> x ^. SectionParent ==. p ?. SectionId)
        where_ $ x ^. SectionId ==. val sid
        return (x,p)

    (fw0,et0) <- generateFormPost formSectionDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSection 
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/sections/section")


postSectionsR :: Sections -> Handler Html
postSectionsR ps = do
    ((fr,fw),et) <- runFormPost $ formSection Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR $ SectionsR ps
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgSections
              idOverlay <- newIdent
              $(widgetFile "data/sections/new")


getSectionNewR :: Sections -> Handler Html
getSectionNewR ps@(Sections ss) = do
    (fw,et) <- generateFormPost $ formSection Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSections
        idOverlay <- newIdent
        $(widgetFile "data/sections/new")


getSectionsR :: Sections -> Handler Html
getSectionsR ps@(Sections []) = do
    sections <- runDB $ select $ do
        x <- from $ table @Section
        where_ $ isNothing_ $ x ^. SectionParent
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSections
        idOverlay <- newIdent
        $(widgetFile "data/sections/sections")
        
getSectionsR ps@(Sections ss) = do
    sections <- runDB $ select $ do
        x <- from $ table @Section
        where_ $ x ^. SectionParent ==. val (LS.last ss)
        return x

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgSections
        idOverlay <- newIdent
        $(widgetFile "data/sections/subsections")


formSection :: Maybe (Entity Section) -> Form Section
formSection section extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sectionName . entityVal <$> section) 

    sections <- liftHandler $ (bimap unValue unValue <$>) <$> runDB ( select $ do
        x <- from $ table @Section
        orderBy [asc (x ^. SectionName), asc (x ^. SectionId)]
        return (x ^. SectionName, x ^. SectionId) )

    (parentR,parentV) <- mopt (selectFieldList sections) FieldSettings
        { fsLabel = SomeMessage MsgParentSection
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (sectionParent . entityVal <$> section) 

    return ( Section <$> nameR <*> parentR
           , [whamlet|
                     ^{extra}
                     ^{md3widget nameV}
                     ^{md3selectWidget parentV}
                     |]
           )
  where
      uniqueNameField :: Field Handler Text
      uniqueNameField = checkM uniqueName textField

      uniqueName :: Text -> Handler (Either AppMessage Text)
      uniqueName name = do
          x <- runDB $ selectOne $ do
              x <- from $ table @Section
              where_ $ x ^. SectionName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case section of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formSectionDelete :: Form ()
formSectionDelete extra = return (pure (),[whamlet|^{extra}|])
