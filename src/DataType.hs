module DataType
  ( ServerState(..)
  , Handler'(..)
  , Book(..)
  ) where

import qualified Data.Aeson                    as JSON
import qualified Servant                       as Server
import           Universum
import qualified Universum.Unsafe              as Unsafe

data ServerState = ServerState {}
  deriving (Eq, Show)

type Handler' = ReaderT ServerState Server.Handler

data Book = Book
  { title      :: Text
  , date       :: Text
  , categories :: Vector Text
  }
  deriving (Eq, Show, Generic, JSON.FromJSON, JSON.ToJSON)
