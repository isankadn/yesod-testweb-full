module Handler.Post where

import Import

import Yesod.Form.Bootstrap3

import Forms
import Text.Julius(rawJS)

-- how many posts gets displayed
-- this will prevent admin from editing posts
-- that are not among this limit but there should not
-- be any reason to edit such old posts
postLimit :: Int
postLimit = 20

getPostsR :: Handler Html
getPostsR = do
  posts <- runDB $ selectList [] [Desc PostCreated, LimitTo postLimit]
  defaultLayout $ do
    setTitleI MsgEditPosts
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "posts")

getAddPostR :: Handler Html
getAddPostR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- postForm uid time
  defaultLayout $ do
    setTitleI MsgAddPost
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "add-post")

postAddPostR :: Handler Html
postAddPostR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((result, _), _) <- postForm uid time
  formHandler result $ \post -> do
    runDB $ insert_ post
    setMessageI MsgPostAdded
  redirect AdminR

putPostR :: PostId -> Handler Html
putPostR pid = do
  title <- runInputPost $ ireq textField "title"
  content <- runInputPost $ ireq textareaField "content"
  time <- liftIO getCurrentTime
  runDB $ update pid
    [ PostContent =. content
    , PostTitle =. title
    , PostModified =. Just time
    ]
  redirect PostsR

deletePostR :: PostId -> Handler Html
deletePostR pid = do
  runDB $ delete pid
  redirect PostsR

postForm :: UserId -> UTCTime
  -> Handler ((FormResult Post, Widget), Enctype)
postForm uid time = do
  mr <- getMessageRender
  runFormPost $ renderBootstrap3 BootstrapBasicForm $ Post
    <$> pure uid
    <*> pure time
    <*> pure Nothing
    <*> areq textField (settingsTitle mr) Nothing
    <*> areq textareaField (settingsContent mr) Nothing
    <* bootstrapSubmit (submitButton MsgAddPost)
  where
    settingsTitle mr = withPlaceholder (mr MsgTitle) $ bfs MsgTitle
    settingsContent mr = withRows "10" $ withPlaceholder (mr MsgPost) $ bfs MsgPost