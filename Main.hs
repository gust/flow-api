{-# LANGUAGE OverloadedStrings    #-}

import Migrations
import qualified Web.Scotty as WS
import Control.Monad.Trans
import Control.Monad.State.Class
import TrackerTagging
import PivotalTracker hiding(getApiToken)
import Database.Persist
import Control.Monad.Trans.Reader
import System.Environment(getEnv)
import Control.Monad(liftM)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import App.Environment



connStr = "host=localhost dbname=flow_api user=gust port=5432"

runDbIO statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM statement connection



getApiToken :: IO BCH.ByteString
getApiToken = liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"


type ReaderIO = ReaderT Environment IO ()
runReaderIO :: Environment -> ReaderIO -> IO ()
runReaderIO env r = runReaderT r env
main :: IO ()
main =  do
  apiToken <- getApiToken
  runDbIO $ runMigrationUnsafe migrateAll
  port <- liftM read $ getEnv "PORT"
  let environment = Environment apiToken
  WS.scotty port $ do
    WS.post "/" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO . (runReaderIO environment) $ do 
        ((getStories gitLog) >>= (updateLabelsOnStories label)) 
       WS.html "<h1>success</h1>"
    {- WS.post "/releases" $ do -}
       {- gitLog <- WS.param "git_log" -}
       {- app    <- WS.param "app" -}
       {- let label = "deployed to " ++ (lazyByteStringToString app) -}
       {- pivotalStories <- liftIO $ getStories gitLog -}
       {- (liftIO $ updateLabelsOnStories label pivotalStories) >> WS.html "<h1>success</h1>" -}
       {- runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do -}
          {- liftIO $ runSqlPersistM (insertMany pivotalStories) connection -}
       {- return () -}
