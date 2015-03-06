import Control.Arrow
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Database.Redis (runRedis)
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Redis as Redis


data Match = Match
    { guid :: Text
    , creatorId :: Text
    , opponentId :: Text
    , creatorScore :: Int
    , opponentScore :: Int
    }
  deriving (Generic, Show)

instance FromJSON Match
instance ToJSON Match

type MatchAPI =
         Get Text
    :<|> "matches" :> Get (Either (Int, String) [Match])
    :<|> "match" :> ReqBody Match :> Post Match

noteAPI :: Proxy MatchAPI
noteAPI =
    Proxy

getMatches :: MonadIO m => Redis.Connection -> m (Either (Int,String) [Match])
getMatches redisConn = do
    matchPkeys' <- liftIO $ runRedis redisConn $ Redis.smembers "matches"
    let matchPkeys = either (error . show) id matchPkeys'
    ms <- mapM pkeyToMatch matchPkeys
    return (Right ms)
    where pkeyToMatch pkey = do
            Right attrs' <- liftIO $ runRedis redisConn $ Redis.hmget pkey ["creator_id", "opponent_id", "creator_score", "opponent_score"]
            let [cid, oid, cs, os] = catMaybes attrs'
            return $ Match (T.decodeUtf8 pkey) (T.decodeUtf8 cid) (T.decodeUtf8 oid) (read . B.unpack $ cs) (read . B.unpack $ os)

postMatch :: MonadIO m => Redis.Connection -> Match -> m Match
postMatch redisConn match =
    liftIO $ do
      let attrs = [ ("creator_id", creatorId match)
                  , ("opponent_id", opponentId match)
                  , ("creator_score", T.pack . show $ creatorScore match)
                  , ("opponent_score", T.pack . show $ opponentScore match)
                  ]
          pkey = "match:" <> T.encodeUtf8 (guid match)
      _ <- liftIO $ runRedis redisConn $ Redis.hmset pkey (map (second T.encodeUtf8) attrs)
      _ <- liftIO $ runRedis redisConn $ Redis.sadd "matches" [pkey]
      return match

server :: Text -> Redis.Connection -> Server MatchAPI
server home conn =
         return home
    :<|> getMatches conn
    :<|> postMatch conn

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = T.decodeUtf8 . toStrict . encode $ Map.fromList [("success" :: Text, True)]
    redisConn <- Redis.connect Redis.defaultConnectInfo
    run port $ serve noteAPI $ server home redisConn
