{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Locations 
  ( getLocationsR, postLocationsR
  , getLocationR, postLocationR
  , getLocationNewR
  , getLocationEditR
  , postLocationDeleR
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
    , DataR (LocationNewR, LocationR, LocationsR, LocationEditR, LocationDeleR)
    , AppMessage
      ( MsgLocation, MsgLocations, MsgNoDataYet, MsgName, MsgAlreadyExists
      , MsgSave, MsgCancel, MsgRecordAdded, MsgDeleteAreYouSure, MsgDele
      , MsgConfirmPlease, MsgRecordEdited, MsgRecordDeleted, MsgInvalidFormData
      )
    )
    
import Material3 (md3widget)

import Model
    ( msgSuccess, msgError
    , Location(Location, locationName), LocationId
    , EntityField (LocationName, LocationId)
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


postLocationDeleR :: LocationId -> Handler Html
postLocationDeleR lid = do
    ((fr,_),_) <- runFormPost formLocationDelete
    case fr of
      FormSuccess () -> do
          runDB $ delete lid
          addMessageI msgSuccess MsgRecordDeleted
          redirect $ DataR LocationsR
      _otherwise -> do
          addMessageI msgError MsgInvalidFormData
          redirect $ DataR LocationsR


postLocationR :: LocationId -> Handler Html
postLocationR lid = do
    typ <- runDB $ selectOne $ do
        x <- from $ table @Location
        where_ $ x ^. LocationId ==. val lid
        return x
    
    ((fr,fw),et) <- runFormPost $ formLocation typ
    case fr of
      FormSuccess r -> do
          runDB $ replace lid r
          addMessageI msgSuccess MsgRecordEdited
          redirect $ DataR $ LocationR lid
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgLocation
              idOverlay <- newIdent
              $(widgetFile "data/pump/locations/edit")


getLocationEditR :: LocationId -> Handler Html
getLocationEditR lid = do
    location <- runDB $ selectOne $ do
        x <- from $ table @Location
        where_ $ x ^. LocationId ==. val lid
        return x
    
    (fw,et) <- generateFormPost $ formLocation location

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgLocation
        idOverlay <- newIdent
        $(widgetFile "data/pump/locations/edit")


getLocationR :: LocationId -> Handler Html
getLocationR lid = do
    location <- runDB $ selectOne $ do
        x <- from $ table @Location
        where_ $ x ^. LocationId ==. val lid
        return x

    (fw0,et0) <- generateFormPost formLocationDelete

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgLocation
        idOverlay <- newIdent
        idDialogDelete <- newIdent
        $(widgetFile "data/pump/locations/location")


postLocationsR :: Handler Html
postLocationsR = do
    ((fr,fw),et) <- runFormPost $ formLocation Nothing
    case fr of
      FormSuccess r -> do
          runDB $ insert_ r
          addMessageI msgSuccess MsgRecordAdded
          redirect $ DataR LocationsR
      _otherwise -> do
          msgr <- getMessageRender
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgLocations
              idOverlay <- newIdent
              $(widgetFile "data/pump/locations/new")


getLocationNewR :: Handler Html
getLocationNewR = do
    (fw,et) <- generateFormPost $ formLocation Nothing

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgLocations
        idOverlay <- newIdent
        $(widgetFile "data/pump/locations/new")


getLocationsR :: Handler Html
getLocationsR = do
    types <- runDB $ select $ from $ table @Location

    msgr <- getMessageRender
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgLocations
        idOverlay <- newIdent
        $(widgetFile "data/pump/locations/locations")


formLocation :: Maybe (Entity Location) -> Form Location
formLocation typ extra = do

    (nameR,nameV) <- mreq uniqueNameField FieldSettings
        { fsLabel = SomeMessage MsgName
        , fsId = Nothing, fsName = Nothing, fsTooltip = Nothing, fsAttrs = []
        } (locationName . entityVal <$> typ) 

    return ( Location <$> nameR
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
              x <- from $ table @Location
              where_ $ x ^. LocationName ==. val name
              return x
          return $ case x of
            Nothing -> Right name
            Just (Entity rid _) -> case typ of
              Nothing -> Left MsgAlreadyExists
              Just (Entity rid' _) | rid == rid' -> Right name
                                   | otherwise -> Left MsgAlreadyExists


formLocationDelete :: Form ()
formLocationDelete extra = return (pure (),[whamlet|^{extra}|])
