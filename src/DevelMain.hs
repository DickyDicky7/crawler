module DevelMain
  ( update
  ) where

import qualified Rapid                         as Devel
import           Server                         ( start )
import           Universum
import qualified Universum.Unsafe              as Unsafe

update :: IO ()
update = Devel.rapid 0 \client -> Devel.restart client "webserver" start
