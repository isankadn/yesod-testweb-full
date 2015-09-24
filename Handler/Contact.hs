module Handler.Contact where

import Import

import Helpers
import Calendar

getContactR :: Handler Html
getContactR = do
  day <- liftIO today
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "contact")
