import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Database.Redis (Redis, Reply, runRedis)
import qualified Database.Redis as Redis
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T


newtype Note = Note
    { contents :: Text
    }
  deriving (Generic, Show)

instance FromJSON Note
instance ToJSON Note


getNotes :: MonadIO m => Redis.Connection -> m [Note]
getNotes redisConn = do
    let redisCmd = Redis.lrange "foo" 0 (-1) :: Redis (Either Reply [ByteString])
    ts' <- liftIO (runRedis redisConn redisCmd)
    let ts = either (error . show) id ts'
    return $ map (Note . T.decodeUtf8) ts

postNote :: MonadIO m => Redis.Connection -> Note -> m [Note]
postNote redisConn note =
    liftIO $ do
      T.putStrLn $ contents note
      _ <- liftIO $ runRedis redisConn $ Redis.lpush "foo" [T.encodeUtf8 $ contents note]
      return []


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody Note :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

server :: Text -> Redis.Connection -> Server NoteAPI
server home conn =
         return home
    :<|> getNotes conn
    :<|> postNote conn


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome to Haskell on Heroku" T.pack $
                 lookup "TUTORIAL_HOME" env
    redisConn <- Redis.connect Redis.defaultConnectInfo
    run port $ serve noteAPI $ server home redisConn
