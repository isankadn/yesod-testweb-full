module Handler.User where

import Import

import Yesod.Form.Bootstrap3
import Yesod.Auth.HashDB(setPassword)

import Forms
import Data.Maybe

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Asc UserName]
  defaultLayout $ do
    setTitleI MsgUsers
    $(widgetFile "users")

getUserR :: UserId -> Handler Html
getUserR uid = do
  user <- runDB $ get404 uid
  ((_, formWidget), formEnctype) <- runFormPost $ editUserForm user
  defaultLayout $ do
    setTitleI MsgUser
    $(widgetFile "user")

postUserR :: UserId -> Handler Html
postUserR uid = do
  user <- runDB $ get404 uid
  ((result, _), _) <- runFormPost $ editUserForm user
  formHandler result $ \user -> do
    runDB $ replace uid user
    setMessageI MsgUserUpdated
  redirect $ UserR uid

editUserForm :: User -> Form User
editUserForm user extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) (Just $ userName user)
  (emailRes, emailView) <- mopt emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) (Just $ userEmail user)
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAdmin) Nothing Nothing Nothing
      []) (Just $ userAdmin user)
  (superAdminRes, superAdminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgSuperAdmin) Nothing Nothing Nothing
      []) (Just $ userSuperAdmin user)
  let result = User <$> nameRes
                    <*> emailRes
                    <*> pure (userPassword user)
                    <*> adminRes
                    <*> superAdminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel emailView}
          ^{fvInput emailView}
        <div .c-inputs-stacked>
          <label .c-input .c-checkbox>
            ^{fvInput adminView}^{fvLabel adminView}
            <span .c-indicator>
          <label .c-input .c-checkbox>
            ^{fvInput superAdminView}^{fvLabel superAdminView}
            <span .c-indicator>
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgSave}>
      |]
  return (result, widget)

getAddUserR :: Handler Html
getAddUserR = do
  ((_, formWidget), formEnctype) <- runFormPost newUserForm
  defaultLayout $ do
    setTitleI MsgAddUser
    $(widgetFile "add-user")

postAddUserR :: Handler Html
postAddUserR = do
  ((result, _), _) <- runFormPost newUserForm
  formHandler result $ \user -> do
    runDB $ insert_ =<< setPassword (fromJust $ userPassword user) user
    setMessageI MsgUserAdded
  redirect AdminR

newUserForm :: Form User
newUserForm extra = do
  mr <- getMessageRender
  (nameRes, nameView) <- mreq textField
    (withPlaceholder (mr MsgName) $ bfs MsgName) Nothing
  (emailRes, emailView) <- mopt emailField
    (withPlaceholder (mr MsgEmail) $ bfs MsgEmail) Nothing
  (pwRes, pwView) <- mopt textField
    (withPlaceholder (mr MsgPassword) $ bfs MsgPassword) Nothing
  (adminRes, adminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgAdmin) Nothing Nothing Nothing
      []) Nothing
  (superAdminRes, superAdminView) <- mreq checkBoxField
    (FieldSettings (SomeMessage MsgSuperAdmin) Nothing Nothing Nothing
      []) Nothing
  let result = User <$> nameRes
                    <*> emailRes
                    <*> pwRes
                    <*> adminRes
                    <*> superAdminRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel nameView}
          ^{fvInput nameView}
        <div .form-group>
          <label .control-label>^{fvLabel emailView}
          ^{fvInput emailView}
        <div .form-group>
          <label .control-label>^{fvLabel pwView}
          ^{fvInput pwView}
        <div .c-inputs-stacked>
          <label .c-input .c-checkbox>
            ^{fvInput adminView}^{fvLabel adminView}
            <span .c-indicator>
          <label .c-input .c-checkbox>
            ^{fvInput superAdminView}^{fvLabel superAdminView}
            <span .c-indicator>
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgAddUser}>
      |]
  return (result, widget)