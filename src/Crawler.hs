module Crawler
  ( start
  ) where

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.Pretty.Simple
import           Text.XML
import           Text.XML.Cursor

start :: IO ()
start = do
  albums <- getPitchforkAlbums
  encodeFile "src/album.json" albums

data Album = Album
  { artist :: Text
  , title  :: Text
  , date   :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

getPitchforkAlbums :: IO [Album]
getPitchforkAlbums =
  httpLBS "https://pitchfork.com/rss/reviews/albums/" >>= \response ->
    let
        ---
        document :: Document
        document = parseLBS_ def (getResponseBody response)

        cursor :: Cursor
        cursor = fromDocument document

        titles :: [Text]
        titles = cursor $// element "item" &/ element "title" &// content

        toAlbumList :: Text -> [[Text]]
        toAlbumList = map (T.splitOn ": ") . T.splitOn " // "

        albumList :: [[Text]]
        albumList = concatMap toAlbumList titles

        toAlbumAwaitingDate :: [Text] -> (Text -> Album)
        toAlbumAwaitingDate [artist, name] = Album artist name
        toAlbumAwaitingDate _              = error "Unknown format"

        dates :: [Text]
        dates = cursor $// element "item" &/ element "pubDate" &// content

        toUTCTime :: Text -> Maybe UTCTime
        toUTCTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z" . T.unpack

        toDate :: Text -> Text
        toDate = toUTCTime >>= \case
          Nothing   -> return ""
          Just date -> return (T.pack (formatTime defaultTimeLocale "%b %d" date))

        dateList :: [Text]
        dateList = map toDate dates

        albums :: [Album]
        albums = zipWith toAlbumAwaitingDate albumList dateList
        ---
    in  pure albums

