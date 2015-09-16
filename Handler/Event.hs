module Handler.Event where

import Import

import Text.Julius(rawJS)

-- how many events gets displayed
-- this will prevent admin from editing events
-- that are not among this limit but there should not
-- be any reason to edit such old events
eventLimit :: Int
eventLimit = 20

getEventsR :: Handler Html
getEventsR = do
  events <- runDB $ selectList [] [Desc EventCreated, LimitTo eventLimit]
  defaultLayout $ do
    setTitleI MsgEditEvents
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "events")

putEventR :: EventId ->  Handler Html
putEventR eid = error "Not yet implemented: putEventR"

deleteEventR :: EventId -> Handler Html
deleteEventR eid = error "Not yet implemented: deleteEventR"

getAddEventR :: Handler Html
getAddEventR = error "Not yet implemented: getAddEventR"

postAddEventR :: Handler Html
postAddEventR = error "Not yet implemented: postAddEventR"