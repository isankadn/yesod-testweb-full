module Handler.Courses where

import Import

getCoursesR :: Handler Html
getCoursesR = do
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "courses")
