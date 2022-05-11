module Crawler
    ( start
    ) where

import           Data.Aeson
import           Data.Text               hiding ( concat
                                                , concatMap
                                                , map
                                                )
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
        --
    in  do
            pPrint albumList

  where


    -- toAlbumAwaitingDate :: Array Int Text -> (Text -> Album)
    -- toAlbumAwaitingDate [a, t]      = Album a t
    -- toAlbumAwaitingDate [a]         = Album a ""
    -- toAlbumAwaitingDate [a, t1, t2] = Album a (t1 <> ": " <> t2)
    -- toAlbumAwaitingDate _           = error "Unknown format"

