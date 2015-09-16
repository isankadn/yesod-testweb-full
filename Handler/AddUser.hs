module Handler.AddUser where

import Import

import Yesod.Form.Bootstrap3
import Yesod.Auth.HashDB(setPassword)

import Forms

import Data.Maybe

getAddUserR :: Handler Html
getAddUserR = do
  ((_, formWidget), formEnctype) <- runFormPost userForm
  defaultLayout $ do
    setTitleI MsgAddUser
    $(widgetFile "add-user")

postAddUserR :: Handler Html
postAddUserR = do
  ((result, _), _) <- runFormPost userForm
  formHandler result $ \user -> do
    runDB $ insert_ =<< setPassword (fromJust $ userPassword user) user
    setMessageI MsgUserAdded
  redirect AdminR

userForm :: Form User
userForm extra = do
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