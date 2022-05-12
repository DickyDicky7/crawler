module Crawler
    ( start
    ) where

import           Data.Aeson
import           Data.Text               hiding ( concat
                                                , concatMap
                                                , map
                                                , zipWith
                                                )
import           Data.Time
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.Pretty.Simple
import           Text.XML
import           Text.XML.Cursor

data Album = Album
    { artist :: Text
    , title  :: Text
    , date   :: Text
    }
    deriving (Eq, Show, Generic, ToJSON)

start :: IO ()
start = httpLBS "https://pitchfork.com/rss/reviews/albums/" >>= \response ->
    let
        --
        document :: Document
        document = parseLBS_ def (getResponseBody response)

        cursor :: Cursor
        cursor = fromDocument document

        titles :: [Text]
        titles = cursor $// element "item" &/ element "title" &// content

        toAlbumList :: Text -> [[Text]]
        toAlbumList = map (splitOn ": ") . splitOn " // "

        albumList :: [[Text]]
        albumList = concatMap toAlbumList titles

        toAlbumAwaitingDate :: [Text] -> (Text -> Album)
        toAlbumAwaitingDate [artist, name] = Album artist name
        toAlbumAwaitingDate _              = error "Unknown format"

        dates :: [Text]
        dates = cursor $// element "item" &/ element "pubDate" &// content

        toUTCTime :: Text -> Maybe UTCTime
        toUTCTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z" . unpack

        toDate :: Text -> Text
        toDate = toUTCTime >>= \case
            Nothing   -> return ""
            Just date -> return (pack (formatTime defaultTimeLocale "%b %d" date))

        dateList :: [Text]
        dateList = map toDate dates

        albums :: [Album]
        albums = zipWith toAlbumAwaitingDate albumList dateList
        --
    in  do
            encodeFile "src/album.json" albums



