module Handler.Image where

import Import

import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

postImageR :: Handler Html
postImageR = do
  mfile <- lookupHeader "filename"
  root <- liftM (appRoot . appSettings) getYesod
  case mfile of
    Just file -> do
      bs <- rawRequestBody $$ CL.consume
      liftIO $ B.writeFile (B8.unpack ("images/" ++ file)) $ B.concat bs
      sendResponse $ (unpack root) ++ "/images/" ++ B8.unpack file
    Nothing -> invalidArgs []