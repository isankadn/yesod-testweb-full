module Handler.Contact where

import Import

getContactR :: Handler Html
getContactR = do
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "contact")
