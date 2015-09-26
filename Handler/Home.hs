module Handler.Home where

import Import

import Data.Time.LocalTime

import Helpers
import Calendar
import Model.Post

-- how many posts get displayed in the front page
postsPerPage :: Int
postsPerPage = 10

-- convenience handler for the user
-- meaning "/" will be the same as "/1"
getHomeR :: Handler Html
getHomeR = getHomePageR 1

getHomePageR :: Int -> Handler Html
getHomePageR page = do
  posts <- runDB $ getPosts page postsPerPage
  tz <- liftIO getCurrentTimeZone
  widget <- calendarWidget
  defaultLayout $ do
    setTitle "Tampereen Frisbeeseura"
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "home")