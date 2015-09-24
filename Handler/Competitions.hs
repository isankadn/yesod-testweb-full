module Handler.Competitions where

import Import

import Helpers
import Calendar

getCompetitionsR :: Handler Html
getCompetitionsR = do
  day <- liftIO today
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "competitions")