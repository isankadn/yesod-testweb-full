module Handler.Companies where

import Import

import Helpers
import Calendar
import Model.Event

getCompaniesR :: Handler Html
getCompaniesR = do
  widget <- calendarWidget
  defaultLayout $ do
    setTitleI MsgCompanies
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "calendar")
    $(widgetFile "companies")