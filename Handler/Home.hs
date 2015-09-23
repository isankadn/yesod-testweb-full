module Handler.Home where

import Import

import Data.Time.LocalTime
import Text.Blaze

import Helpers

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
  defaultLayout $ do
    setTitle "Tampereen Frisbeeseura"
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "home")

getPosts :: Int -> DB [Entity Post]
getPosts page = selectList
  []
  [ Desc PostCreated
  , LimitTo postsPerPage
  , OffsetBy $ (page - 1) * postsPerPage
  ]

getEvents :: DB [Entity Event]
getEvents = selectList
  []
  [ Desc EventStartDate
  ]