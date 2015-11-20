module App.Environment(loadEnvironment, Environment(..)) where
import qualified Data.ByteString.Char8 as BCH
import Control.Monad(liftM, liftM5)
import System.Environment(getEnv)

data Environment = Environment {
  pivotalTrackerApiToken :: BCH.ByteString
  , slackEndpoint :: String
  , connectionString :: BCH.ByteString
  , appPort :: Int
}

parsePostgresConnectionUrl :: String -> String -> String -> String -> String -> BCH.ByteString
parsePostgresConnectionUrl host dbname user password port = BCH.pack $ "host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password ++ " port=" ++ port

loadEnvironment :: IO Environment
loadEnvironment = do
  apiToken <- BCH.pack `liftM` getEnv "PIVOTAL_TRACKER_API_TOKEN"
  connectionString <- liftM5 parsePostgresConnectionUrl (getEnv "DATABASE_HOST") (getEnv "DATABASE_NAME") (getEnv "DATABASE_USER") (getEnv "DATABASE_PASSWORD") (getEnv "DATABASE_PORT")
  port <- read `liftM` getEnv "PORT"
  slackEndpoint <- getEnv "SLACK_INTEGRATION_ENDPOINT"
  return $ Environment apiToken slackEndpoint connectionString port
