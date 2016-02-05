{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Main where
import qualified Network.Slack.Api as Slack
import Options.Applicative 
import Control.Applicative
import Network.HTTP.Types.URI (parseQuery)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.WebSockets
import Data.Aeson
import Network.URI
import Control.Monad (join)

data Options = Options {
    token :: String
  } deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions = Options 
    <$> strOption (long "token" <> short 't' <> metavar "TOKEN" <> help "Auth token")

opts = info (helper <*> parseOptions)
            (fullDesc 
             <> progDesc "Connect to Slack API wss"
             <> header "slack-websocket-client")

main = do
    Options{..} <- execParser opts
    r :: Slack.SlackResponse <- Slack.request token "rtm.start" []
    case r of 
      Slack.Success bs -> connectWS bs
      x -> error $ "API response error: " ++ show x

connectWS :: L8.ByteString -> IO ()
connectWS bs = do
    let r :: Maybe RTMResponse = decode bs
        url :: Maybe String
        url = rtmResponseWebsocketURL <$> r
        uri = join $ parseURI <$> url
    case uri of
      Just uri' -> do
          print uri'
          let protocol = uriScheme uri'  -- wss:
              host = maybe
                       (error $ "Could not parse authority for URI: " ++ show uri')
                       id $ uriRegName <$> uriAuthority uri'
              path  = uriPath uri'
          connectSlackWebsock protocol host  path
      Nothing -> error $ "Could not parse ws url: " ++ show url


connectSlackWebsock :: String -> String -> String -> IO ()
connectSlackWebsock protocol host path = do
  print [protocol,host,path]
  _ <- runClient host 80 path clientApp 
  return ()
  
-- type ClientApp a = Connection -> IO a

-- TODO try to read a stream of messages 
clientApp :: ClientApp ()
clientApp c = do
    m <- receive c
    print m


data RTMResponse = RTMResponse {
      rtmResponseWebsocketURL :: String
      } deriving Show

instance FromJSON RTMResponse where
  parseJSON (Object v) = RTMResponse <$> v .: "url"
  parseJSON x = error $ "Could not parseJSON " ++ show x

  
