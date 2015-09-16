module ErrorHandlers where

import Import.NoFoundation
import Control.Monad.Logger (logErrorS)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Network.Wai as W
import qualified Data.ByteString.Char8 as S8

-- | The default error handler for 'errorHandler'.
defaultErrorHandler :: Yesod site => ErrorResponse -> HandlerT site IO TypedContent
defaultErrorHandler NotFound = selectRep $ do
  provideRep $ defaultLayout $ do
    r <- waiRequest
    let path' = TE.decodeUtf8With TEE.lenientDecode $ W.rawPathInfo r
    setTitle "Not Found"
    toWidget [hamlet|
      <div .m-t .container>
        <h1>Not Found
        <p>#{path'}
    |]
  provideRep $ return $ object ["message" .= ("Not Found" :: Text)]

-- For API requests.
-- For a user with a browser,
-- if you specify an authRoute the user will be redirected there and
-- this page will not be shown.
defaultErrorHandler NotAuthenticated = selectRep $ do
  provideRep $ defaultLayout $ do
    setTitle "Not logged in"
    toWidget [hamlet|
      <div .m-t .container>
        <h1>Not logged in
        <p style="display:none;">Set the authRoute and the user will be redirected there.
    |]

  provideRep $ do
    -- 401 *MUST* include a WWW-Authenticate header
    -- however, there is no standard to indicate a redirection
    --
    -- change this to Basic or Digest if you allow those forms of authentications
    addHeader "WWW-Authenticate" "RedirectJSON realm=\"application\", param=\"authentication_url\""

    -- The client will just use the authentication_url in the JSON
    site <- getYesod
    rend <- getUrlRender
    return $ object $ [
      "message" .= ("Not logged in" :: Text)
      ] ++
      case authRoute site of
        Nothing -> []
        Just url -> ["authentication_url" .= rend url]

defaultErrorHandler (PermissionDenied msg) = selectRep $ do
  provideRep $ defaultLayout $ do
    setTitle "Permission Denied"
    toWidget [hamlet|
      <div .m-t .container>
        <h1>Permission denied
        <p>#{msg}
    |]
  provideRep $
    return $ object $ [
      "message" .= ("Permission Denied. " <> msg)
      ]

defaultErrorHandler (InvalidArgs ia) = selectRep $ do
  provideRep $ defaultLayout $ do
    setTitle "Invalid Arguments"
    toWidget [hamlet|
      <div .m-t .container>
        <h1>Invalid Arguments
        <ul>
          $forall msg <- ia
              <li>#{msg}
    |]
  provideRep $ return $ object ["message" .= ("Invalid Arguments" :: Text), "errors" .= ia]

defaultErrorHandler (InternalError e) = do
  $logErrorS "yesod-core" e
  selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle "Internal Server Error"
      toWidget [hamlet|
        <div .m-t .container>
          <h1>Internal Server Error
          <pre>#{e}
      |]
    provideRep $ return $ object ["message" .= ("Internal Server Error" :: Text), "error" .= e]

defaultErrorHandler (BadMethod m) = selectRep $ do
  provideRep $ defaultLayout $ do
    setTitle "Bad Method"
    toWidget [hamlet|
      <div .m-t .container>
        <h1>Method Not Supported
        <p>Method <code>#{S8.unpack m}</code> not supported
    |]
  provideRep $ return $ object ["message" .= ("Bad method" :: Text), "method" .= TE.decodeUtf8With TEE.lenientDecode m]