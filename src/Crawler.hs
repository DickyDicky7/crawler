module Crawler
  ( start
  ) where

import qualified Control.Concurrent.Async      as Async
import qualified Data.Aeson                    as JSON
import qualified Data.ByteString               as Strict
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Time                      ( UTCTime )
import qualified Data.Time                     as Time
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Split             as Vector.Util
import qualified Network.HTTP.Simple           as Network
import           Text.Regex.TDFA
import           Text.XML                       ( Document )
import qualified Text.XML                      as XML
import           Text.XML.Cursor
import           Universum
import qualified Universum.Unsafe              as Unsafe

data Book = Book
  { title      :: Text
  , date       :: Text
  , categories :: Vector Text
  }
  deriving (Eq, Show, Read, Generic, JSON.FromJSON, JSON.ToJSON)

start :: IO ()
start = requestList `Async.forConcurrently_` \request -> do
  books <- getBooks (Network.parseRequest_ (decodeUtf8 request))
  getFilePath request `writeBooksJSON` books

requestList :: Vector ByteString
requestList =
  [ "http://www.publicbooks.org/tag/global-black-history/feed"
  , "http://www.publicbooks.org/tag/on-our-nightstands/feed"
  , "http://www.publicbooks.org/tag/the-big-picture/feed"
  , "http://www.publicbooks.org/tag/public-streets/feed"
  , "http://www.publicbooks.org/tag/climate-change/feed"
  , "http://www.publicbooks.org/tag/translation/feed"
  , "http://www.publicbooks.org/tag/politics/feed"
  , "http://www.publicbooks.org/tag/fiction/feed"
  , "http://www.publicbooks.org/tag/gender/feed"
  , "http://www.publicbooks.org/feed"
  -- ...
  ]

getFilePath :: ByteString -> String
getFilePath = \case
  "http://www.publicbooks.org/feed" -> "json/overall.json"
  request -> decodeUtf8 ("json/" <> Strict.take (length request - 36) (Strict.drop 31 request) <> ".json")

writeBooksJSON :: FilePath -> Vector Book -> IO ()
writeBooksJSON path books = path `Strict.writeFile` Lazy.toStrict (toBooksJSON books)

toBooksJSON :: Vector Book -> LByteString
toBooksJSON = JSON.encode

fromBooksJSON :: LByteString -> Vector Book
fromBooksJSON = fromMaybe [] . JSON.decode

getBooks :: Network.Request -> IO (Vector Book)
getBooks request = getXMLCursor request >>= getXMLData >>= splitXMLData >>= processXMLData

getXMLCursor :: Network.Request -> IO Cursor
getXMLCursor = Network.httpLBS >=> pure . fromDocument . parseDocument
 where
  ---
  parseDocument :: Network.Response LByteString -> Document
  parseDocument = XML.parseLBS_ XML.def . Network.getResponseBody
  ---

getXMLData :: Cursor -> IO (Vector Text)
getXMLData = pure . Vector.fromList . ($// element "item" >=> child &// content)

splitXMLData :: Vector Text -> IO (Vector (Vector Text))
splitXMLData = pure . Vector.filter ((> 1) . Vector.length) . Vector.fromList . Vector.Util.split
  (Vector.Util.whenElt (=~ matchPattern))
 where
  ---
  matchPattern :: Text
  matchPattern = "<p>"
  ---

processXMLData :: Vector (Vector Text) -> IO (Vector Book)
processXMLData = pure . Vector.map toBook

toBook :: Vector Text -> Book
toBook = (\[[title, date], categories] -> Book { date = toDate date, .. })
  . Vector.sequence [Vector.sequence [(Vector.! 0), (Vector.! 3)], getCategories]
 where
  ---
  getCategories :: Vector Text -> Vector Text
  getCategories bookData = Vector.slice 4 (Vector.length bookData - 5) bookData
  ---

toUTCTime :: Text -> Maybe UTCTime
toUTCTime = Time.parseTimeM True Time.defaultTimeLocale "%a, %d %b %Y %X %z" . toString

toDate :: Text -> Text
toDate = maybe "" (toText . Time.formatTime Time.defaultTimeLocale "%d-%m-%Y") . toUTCTime
