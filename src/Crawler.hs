module Crawler
  ( start
  ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString               as BS
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Vector.Split
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.Pandoc.UTF8
-- import           System.Directory
-- import           Text.Pretty.Simple
import           Text.Regex.TDFA
import           Text.XML
import           Text.XML.Cursor

data Book = Book
  { title      :: Text
  , date       :: Text
  , categories :: Vector Text
  }
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

start :: IO ()
start = forConcurrently_
  requestList
  \request -> do
    books <- getBooks (parseRequest_ (toString request))
    getFilePath request `writeBooksJSON'` books

requestList :: Vector BS.ByteString
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

getFilePath :: BS.ByteString -> String
getFilePath = \case
  "http://www.publicbooks.org/feed" -> "json/overall.json"
  request                           -> toString
    (BS.append
      (BS.append "json/" (BS.take (BS.length request - 36) (BS.drop 31 request)))
      ".json"
    )

writeBooksJSON' :: FilePath -> Vector Book -> IO ()
writeBooksJSON' path books = path `BS.writeFile` BL.toStrict (toBooksJSON books)

writeBooksJSON :: FilePath -> Vector Book -> IO ()
writeBooksJSON path books = do
  contents <- BS.readFile path
  if BS.null contents
    then path `BS.writeFile` BL.toStrict (toBooksJSON books)
    else do
      let books' = fromBooksJSON (BL.fromStrict contents) V.++ books
      path `BS.writeFile` BL.toStrict (toBooksJSON books')

toBooksJSON :: Vector Book -> ByteString
toBooksJSON = encode

fromBooksJSON :: ByteString -> Vector Book
fromBooksJSON = fromMaybe [] . decode

getBooks :: Request -> IO (Vector Book)
getBooks request =
  getXMLCursor request >>= getXMLData >>= splitXMLData >>= processXMLData

getXMLCursor :: Request -> IO Cursor
getXMLCursor = httpLBS >=> pure . fromDocument . parseDocument
 where
  ---
  parseDocument :: Response ByteString -> Document
  parseDocument = parseLBS_ def . getResponseBody
  ---

getXMLData :: Cursor -> IO (Vector Text)
getXMLData = pure . V.fromList . ($// element "item" >=> child &// content)

splitXMLData :: Vector Text -> IO (Vector (Vector Text))
splitXMLData = pure . V.filter ((> 1) . V.length) . V.fromList . split
  (whenElt (=~ matchPattern))
 where
  ---
  matchPattern :: Text
  matchPattern = "<p>"
  ---

processXMLData :: Vector (Vector Text) -> IO (Vector Book)
processXMLData = pure . V.map toBook

toBook :: Vector Text -> Book
toBook = (\[[title, date], categories] -> Book { date = toDate date, .. })
  . V.sequence [V.sequence [(V.! 0), (V.! 3)], getCategories]
 where
  ---
  getCategories :: Vector Text -> Vector Text
  getCategories bookData = V.slice 4 (V.length bookData - 5) bookData
  ---

toUTCTime :: Text -> Maybe UTCTime
toUTCTime = parseTimeM True defaultTimeLocale "%a, %d %b %Y %X %z" . T.unpack

toDate :: Text -> Text
toDate = maybe "" (T.pack . formatTime defaultTimeLocale "%d-%m-%Y") . toUTCTime
