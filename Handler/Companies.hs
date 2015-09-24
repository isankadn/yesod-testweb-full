module Handler.Companies where

import Import

import Helpers
import Calendar

getCompaniesR :: Handler Html
getCompaniesR = do
  day <- liftIO today
  defaultLayout $ do
    setTitleI MsgCompanies
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "companies")