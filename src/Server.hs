module Server
  ( start
  ) where

import           CrawlerServer
import           DataType
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Logger            as Logger
import           Servant
import qualified Servant.Types.SourceT         as Source
import           Universum
import qualified Universum.Unsafe              as Unsafe
import           View

type API = Get '[HTML] Html
  :<|> "json" :> StreamGet NewlineFraming JSON (SourceIO (Vector Book))

requests :: Vector ByteString
requests =
  [ "http://www.publicbooks.org/tag/global-black-history/feed"
  , "http://www.publicbooks.org/tag/on-our-nightstands/feed"
  , "http://www.publicbooks.org/tag/the-big-picture/feed"
  , "http://www.publicbooks.org/tag/public-streets/feed"
  , "http://www.publicbooks.org/tag/climate-change/feed"
  , "http://www.publicbooks.org/tag/translation/feed"
  , "http://www.publicbooks.org/tag/technology/feed"
  , "http://www.publicbooks.org/tag/internet/feed"
  , "http://www.publicbooks.org/tag/politics/feed"
  , "http://www.publicbooks.org/tag/children/feed"
  , "http://www.publicbooks.org/tag/fiction/feed"
  , "http://www.publicbooks.org/tag/gender/feed"
  , "http://www.publicbooks.org/tag/novel/feed"
  , "http://www.publicbooks.org/tag/war/feed"
  , "http://www.publicbooks.org/feed"
  -- ...
  ]

server :: ServerT API Handler'
server = homepageHandler :<|> jsonHandler
 where
  ---
  homepageHandler :: Handler' Html
  homepageHandler = pure homepageView

  jsonHandler :: Handler' (SourceIO (Vector Book))
  jsonHandler = startCrawling requests <&> (Source.source . toList)
  ---

toHandler :: ServerState -> Handler' a -> Handler a
toHandler state handler' = runReaderT handler' state

application :: ServerState -> Application
application state = serve api (hoistServer api (toHandler state) server)
 where
  ---
  api :: Proxy API
  api = Proxy
  ---

start :: IO ()
start = Logger.withStdoutLogger \logger ->
  let settings = Warp.setPort 3000 (logger `Warp.setLogger` Warp.defaultSettings)
  in  Warp.runSettings settings (application ServerState)
