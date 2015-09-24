module Handler.Courses where

import Import

import Helpers
import Calendar

getCoursesR :: Handler Html
getCoursesR = do
  day <- liftIO today
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "courses")
