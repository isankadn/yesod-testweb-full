module Handler.Post where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto((^.))

import Yesod.Form.Bootstrap3
import qualified Data.Text as T
import Text.Julius(rawJS)

import Forms

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
  ((_, formWidget), formEnctype) <- runFormPost $ newPostForm uid time
  defaultLayout $ do
    setTitleI MsgAddPost
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "add-post")

postAddPostR :: Handler Html
postAddPostR = do
  uid <- requireAuthId
  time <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost $ newPostForm uid time
  formHandler result $ \post -> do
    runDB $ insert_ post
    setMessageI MsgPostAdded
  redirect AdminR

postPostR :: PostId -> Handler Html
postPostR pid = do
  post <- runDB $ get404 pid
  time <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost $ editPostForm time post
  formHandler result $ \post -> do
    runDB $ replace pid post
    setMessageI MsgPostEdited
  redirect $ EditPostR pid

deletePostR :: PostId -> Handler Html
deletePostR pid = do
  runDB $ delete pid
  redirect PostsR

getEditPostR :: PostId -> Handler Html
getEditPostR pid = do
  post <- runDB $ get404 pid
  time <- liftIO getCurrentTime
  ((_, formWidget), formEnctype) <- runFormPost $ editPostForm time post
  defaultLayout $ do
    setTitleI MsgEditPost
    mr <- getMessageRender
    $(widgetFile "drag-drop-image")
    $(widgetFile "delete-post")
    $(widgetFile "edit-post")

newPostForm :: UserId -> UTCTime -> Form Post
newPostForm uid time extra = do
  mr <- getMessageRender
  (titleRes, titleView) <- mreq textField
    (withPlaceholder (mr MsgTitle) $ bfs MsgTitle) Nothing
  (contentRes, contentView) <- mreq textareaField
    (withRows "20" $ withPlaceholder (mr MsgPost) $ bfs MsgPost) Nothing
  let result = Post
                <$> pure uid
                <*> pure time
                <*> pure Nothing
                <*> titleRes
                <*> contentRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel titleView}
          ^{fvInput titleView}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgAddPost}>
      |]
  return (result, widget)

editPostForm :: UTCTime -> Post -> Form Post
editPostForm time post extra = do
  mr <- getMessageRender
  (titleRes, titleView) <- mreq textField
    (withPlaceholder (mr MsgTitle) $ bfs MsgTitle) (Just $ postTitle post)
  (contentRes, contentView) <- mreq textareaField
    (withRows "20" $ withPlaceholder (mr MsgPost) $ bfs MsgPost) (Just $ postContent post)
  let result = Post
                <$> pure (postUser post)
                <*> pure (postCreated post)
                <*> pure (Just time)
                <*> titleRes
                <*> contentRes
  let widget = [whamlet|
        #{extra}
        <div .form-group>
          <label .control-label>^{fvLabel titleView}
          ^{fvInput titleView}
        <div .form-group>
          <label .control-label>^{fvLabel contentView}
          ^{fvInput contentView}
        <div .form-group>
          <input type=submit .btn .btn-secondary .btn-block .btn-lg value=_{MsgSave}>
      |]
  return (result, widget)