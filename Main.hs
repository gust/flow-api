{-# LANGUAGE OverloadedStrings    #-}

import Migrations
import qualified Web.Scotty as WS
import Control.Monad.Trans
import Control.Monad.State.Class
import TrackerTagging
import PivotalTracker
import Database.Persist
import System.Environment(getEnv)
import Control.Monad(liftM)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger



connStr = "host=localhost dbname=flow_api user=gust port=5432"

runDbIO statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM statement connection
data Environment = Environment {
    httpPort :: Int
  , pivotalTrackerApiToken :: BCH.ByteString
}

main :: IO ()
main =  do
  apiToken <- getApiToken
  runDbIO $ runMigrationUnsafe migrateAll
  port <- liftM read $ getEnv "PORT"
  WS.scotty port $ do
    let environment = Environment port apiToken
    put environment
    WS.post "/" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       (liftIO $ (getStories gitLog) >>= (updateLabelsOnStories label)) >> (WS.html "<h1>success</h1>")
    WS.post "/releases" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       pivotalStories <- liftIO $ getStories gitLog
       (liftIO $ updateLabelsOnStories label pivotalStories) >> WS.html "<h1>success</h1>"
       runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM (insertMany pivotalStories) connection
       return ()
