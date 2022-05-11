module Crawler
    ( start
    ) where

import           Data.Aeson              hiding ( Array )
import           Data.Array
import           Data.Text               hiding ( map )
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.Pretty.Simple
import           Text.Regex.TDFA
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

        -- getAlbumList :: Text -> Array Int Text
        -- getAlbumList = drop 1 . getCaptureGroups
        -- regex :: Text
        -- regex = "(.*): (.*)"

        -- artistName_Song :: Text -> Array Int Text
        -- artistName_Song = getCaptureGroups regex

        -- artistNames :: [Text]
        -- artistNames = map artistName artists

        dates :: [Text]
        dates = cursor $// element "item" &/ element "pubDate" &// content
        --
    in  do
            pPrint titles

  where

    getCaptureGroups :: Text -> Text -> Array Int Text
    getCaptureGroups regex text = getAllTextSubmatches (text =~ regex)

    at :: Int -> Array Int Text -> Text
    at index matches = matches ! index

    -- toAlbumAwaitingDate :: Array Int Text -> (Text -> Album)
    -- toAlbumAwaitingDate [a, t]      = Album a t
    -- toAlbumAwaitingDate [a]         = Album a ""
    -- toAlbumAwaitingDate [a, t1, t2] = Album a (t1 <> ": " <> t2)
    -- toAlbumAwaitingDate _           = error "Unknown format"

getCaptureGroups :: Text -> Text -> Array Int Text
getCaptureGroups regex text = getAllTextSubmatches (text =~ regex)
