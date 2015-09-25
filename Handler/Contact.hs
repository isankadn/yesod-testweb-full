module Handler.Contact where

import Import

import Helpers
import Calendar

getContactR :: Handler Html
getContactR = do
  widget <- calendarWidget
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "contact")
