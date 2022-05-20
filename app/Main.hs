module Main where

-- import           Chronos.Bench
import           Server                         ( start )
import           Universum
import qualified Universum.Unsafe              as Unsafe

-- main :: IO ()
-- main = defaultMain [benchIO "execute" start]

main :: IO ()
main = start
