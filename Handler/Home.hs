module Handler.Home where

import Import

import Data.Time.LocalTime

import Helpers

import Text.Blaze

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
    p <- selectList
      []
      [ Desc PostCreated
      , LimitTo postsPerPage
      , OffsetBy $ (page - 1) * postsPerPage]
    e <- selectList [] [Desc EventStartTime]
    return (p, e)
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  defaultLayout $ do
    setTitle "TFS"
    $(widgetFile "home")