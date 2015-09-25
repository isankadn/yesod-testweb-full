module Handler.Courses where

import Import

import Helpers
import Calendar

getCoursesR :: Handler Html
getCoursesR = do
  widget <- calendarWidget
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "courses")
