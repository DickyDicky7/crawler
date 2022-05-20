module CrawlerServer
  ( startCrawling
  ) where

import qualified Control.Concurrent.Async      as Async
import qualified Data.ByteString               as Strict
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Time                      ( UTCTime )
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Split             as Vector.Util
import           DataType
import qualified Network.HTTP.Simple           as Network
import           Text.Regex.TDFA
import           Text.XML                       ( Document )
import qualified Text.XML                      as XML
import           Text.XML.Cursor
import           Universum
import qualified Universum.Unsafe              as Unsafe

startCrawling :: Vector ByteString -> Handler' (Vector (Vector Book))
startCrawling = liftIO . Async.mapConcurrently (getBooks . Network.parseRequest_ . decodeUtf8)

getBooks :: Network.Request -> IO (Vector Book)
getBooks request = getXMLCursor request <&> getXMLData <&> splitXMLData <&> processXMLData

getXMLCursor :: Network.Request -> IO Cursor
getXMLCursor = Network.httpLBS >=> pure . fromDocument . parseDocument
 where
  ---
  parseDocument :: Network.Response LByteString -> Document
  parseDocument = XML.parseLBS_ XML.def . Network.getResponseBody
  ---

getXMLData :: Cursor -> Vector Text
getXMLData = Vector.fromList . ($// element "item" >=> child &// content)

splitXMLData :: Vector Text -> Vector (Vector Text)
splitXMLData = Vector.filter ((> 1) . Vector.length) . Vector.fromList . Vector.Util.split
  (Vector.Util.whenElt (=~ matchPattern))
 where
  ---
  matchPattern :: Text
  matchPattern = "<p>"
  ---

processXMLData :: Vector (Vector Text) -> Vector Book
processXMLData = Vector.map toBook

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
