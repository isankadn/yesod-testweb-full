module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import qualified Data.Text as T

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

imagePrefix :: Text
imagePrefix = "image="

toParagraphs :: Post -> [Text]
toParagraphs post = (T.splitOn "\r\n\r\n") $ unTextarea $ postContent post

maybeImage :: Text -> Maybe Text
maybeImage = T.stripPrefix imagePrefix

toArea :: Text -> Textarea
toArea = Textarea