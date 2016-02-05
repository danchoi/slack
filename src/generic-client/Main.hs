{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Main where
import qualified Network.Slack.Api as Slack
import Options.Applicative 
import Control.Applicative
import Network.HTTP.Types.URI (parseQuery)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L8

data Options = Options {
    token :: String
  , endpoint :: String
  , params :: [(String, String)]
  } deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions = Options 
    <$> strOption (long "token" <> short 't' <> metavar "TOKEN" <> help "Auth token")
    <*> strArgument (metavar "ENDPOINT" <> help "endpoint, e.g. channels.list")
    <*> (parseParams <$>
        (strArgument (metavar "PARAMS" <> help "API request params in request query format")))

parseParams :: String -> [(String, String)]
parseParams xs = 
    let xs' = parseQuery $ B.pack xs
    in [(B.unpack a, B.unpack b) | (a,Just b) <- xs']

opts = info (helper <*> parseOptions)
            (fullDesc 
             <> progDesc "Probe the Slack API"
             <> header "slack-generic-client")

main = do
    Options{..} <- execParser opts
    r :: Slack.SlackResponse <- Slack.request token endpoint params
    case r of
        Slack.Success bs -> L8.putStrLn bs
        e -> error $ "response error" ++ show e
    


