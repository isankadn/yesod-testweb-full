module Handler.Competitions where

import Import

getCompetitionsR :: Handler Html
getCompetitionsR = do
  defaultLayout $ do
    $(widgetFile "banner")
    let sidebar = $(widgetFile "sidebar")
    $(widgetFile "competitions")