module Handler.Home where

import Import

import Data.Time.LocalTime
import Text.Blaze

import Helpers
import Calendar
import Model.Event

-- how many posts get displayed in the front page
postsPerPage :: Int
postsPerPage = 10

-- convenience handler for the user
-- meaning "/" will be the same as "/1"
getHomeR :: Handler Html
getHomeR = getHomePageR 1

getHomePageR :: Int -> Handler Html
getHomePageR page = do
  (posts, events) <- runDB $ do
    p <- getPosts page
    e <- getEvents
    return (p, e)
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  widget <- calendarWidget
  defaultLayout $ do
    setTitle "Tampereen Frisbeeseura"
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "home")

getPosts :: Int -> DB [Entity Post]
getPosts page = selectList
  []
  [ Desc PostCreated
  , LimitTo postsPerPage
  , OffsetBy $ (page - 1) * postsPerPage
  ]