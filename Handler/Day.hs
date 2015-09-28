module Handler.Day where

import Import

import Data.Time.LocalTime

import qualified Model.Event as E
import Helpers
import Calendar

getDayR :: Day -> Handler Html
getDayR date = do
  -- let date = fromGregorian year month day
  events <- runDB $ E.getEventsDay date
  tz <- liftIO getCurrentTimeZone
  widget <- calendarWidget
  defaultLayout $ do
    setTitle "Tampereen Frisbeeseura"
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "day")