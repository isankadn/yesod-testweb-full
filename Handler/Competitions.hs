module Handler.Competitions where

import Import

import Helpers
import Calendar

getCompetitionsR :: Handler Html
getCompetitionsR = do
  widget <- calendarWidget
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "competitions")