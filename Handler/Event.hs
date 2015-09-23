module Handler.Event where

import Import

import Yesod.Form.Bootstrap3
import Text.Julius(rawJS)

import Forms
import Helpers
import qualified Datepicker

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

postEventR :: EventId -> Handler Html
postEventR eid = do
  event <- runDB $ get404 eid
  time <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost $ editEventForm time event
  formHandler result $ \event -> do
    if (checkDates (eventStartDate event) (eventEndDate event))
      then do
        runDB $ replace eid event
        setMessageI MsgEventEdited
      else
        setMessageI MsgEndDateInvalid
  redirect $ EditEventR eid

deleteEventR :: EventId -> Handler Html
deleteEventR eid = do
  runDB $ delete eid
  redirect EventsR

getAddEventR :: Handler Html
getAddEventR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- newEventForm uid time
  -- language for datepicker
  lang <- liftM language languages
  defaultLayout $ do
    setTitleI MsgAddEvent
    mr <- getMessageRender
    Datepicker.addDatepicker
    $(widgetFile "datepicker")
    $(widgetFile "drag-drop-image")
    $(widgetFile "add-event")

postAddEventR :: Handler Html
postAddEventR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((result, _), _) <- newEventForm uid time
  formHandler result $ \event -> do
    if (checkDates (eventStartDate event) (eventEndDate event))
      then do
        runDB $ insert_ event
        setMessageI MsgEventAdded
      else
        setMessageI MsgEndDateInvalid
  redirect AdminR

getEditEventR :: EventId -> Handler Html
getEditEventR eid = do
  event <- runDB $ get404 eid
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- runFormPost $ editEventForm time event
  -- language for datepicker
  lang <- liftM language languages
  defaultLayout $ do
    setTitleI MsgEditEvent
    mr <- getMessageRender
    Datepicker.addDatepicker
    $(widgetFile "datepicker")
    $(widgetFile "drag-drop-image")
    $(widgetFile "edit-event")

checkDates :: Day -> Maybe Day -> Bool
checkDates _ Nothing = True
checkDates start (Just end) = start < end

newEventForm :: UserId -> UTCTime
  -> Handler ((FormResult Event, Widget), Enctype)
newEventForm uid time = do
  mr <- getMessageRender
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Event
    <$> pure uid
    <*> pure time
    <*> pure Nothing
    <*> areq textField (settingsTitle mr) Nothing
    <*> areq textField (settingsOrg mr) (Just "Tampereen Frisbeeseura")
    <*> areq textareaField (settingsContent mr) Nothing
    <*> areq dayField (settingsStartTime mr) Nothing
    <*> aopt dayField (settingsEndTime mr) Nothing
    <*> pure Nothing
    <* bootstrapSubmit (submitButton MsgAddEvent)
  where
    settingsTitle mr = withPlaceholder (mr MsgTitle) $ bfs MsgTitle
    settingsOrg mr = withPlaceholder (mr MsgOrganizer) $ bfs MsgOrganizer
    settingsContent mr = withRows "10" $ withPlaceholder (mr MsgEvent) $ bfs MsgEvent
    settingsStartTime mr = withPlaceholder (mr MsgStartDate) $ bfs MsgStartDate
    settingsEndTime mr = withPlaceholder (mr MsgEndDate) $ bfs MsgEndDate

editEventForm :: UTCTime -> Event -> Form Event
editEventForm time event extra = do
  mr <- getMessageRender
  (titleRes, titleView) <- mreq textField
    (withPlaceholder (mr MsgTitle) $ bfs MsgTitle) (Just $ eventTitle event)
  (orgRes, orgView) <- mreq textField
    (withPlaceholder (mr MsgOrganizer) $ bfs MsgOrganizer) (Just $ eventOrganizer event)
  (contentRes, contentView) <- mreq textareaField
    (withRows "10" $ withPlaceholder (mr MsgEvent) $ bfs MsgEvent) (Just $ eventContent event)
  (startRes, startView) <- mreq dayField
    (withPlaceholder (mr MsgStartDate) $ bfs MsgStartDate) (Just $ eventStartDate event)
  (endRes, endView) <- mopt dayField
    (withPlaceholder (mr MsgEndDate) $ bfs MsgEndDate) (Just $ eventEndDate event)
  let result = Event
                <$> pure (eventUser event)
                <*> pure (eventCreated event)
                <*> pure (Just time)
                <*> titleRes
                <*> orgRes
                <*> contentRes
                <*> startRes
                <*> endRes
                <*> pure Nothing
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel titleView}
          ^{fvInput titleView}
        <div .form-group>
          <label .control-label>^{fvLabel orgView}
          ^{fvInput orgView}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <label .control-label>^{fvLabel startView}
          <div .input-group .date>
            ^{fvInput startView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <label .control-label>^{fvLabel endView}
          <div .input-group .date>
            ^{fvInput endView}
            <span .input-group-addon>
              <i .glyphicon .glyphicon-calendar>
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgUpdate}>
      |]
  return (result, widget)