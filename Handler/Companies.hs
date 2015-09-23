module Handler.Companies where

import Import

getCompaniesR :: Handler Html
getCompaniesR = do
  defaultLayout $ do
    setTitleI MsgCompanies
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "companies")