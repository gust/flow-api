{-# LANGUAGE OverloadedStrings    #-}

import Migrations
import qualified Web.Scotty as WS
import Control.Monad.Trans
import Control.Monad.State.Class
import TrackerTagging
import PivotalTracker
import Database.Persist
import Control.Monad.Trans.Reader
import System.Environment(getEnv)
import Control.Monad(liftM)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import App.Environment
import Control.Exception as E
import qualified Network.Wreq as NW


connStr = "host=localhost dbname=flow_api user=gust port=5432"

runDbIO statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM statement connection



instance World IO where
  getWith = NW.getWith
  postWith = NW.postWith
  deleteWith = NW.deleteWith
  tryRequest = E.try


labelStories :: World m => String -> Environment -> BL.ByteString -> m ()
labelStories label environment gitLog = do 
  flip runReaderT environment $ do 
    (getStories gitLog) >>= (updateLabelsOnStories label)

type ReaderIO = ReaderT Environment IO ()
runReaderIO :: Environment -> ReaderIO -> IO ()
runReaderIO env r = runReaderT r env
main :: IO ()
main =  do
  apiToken <- BCH.pack `liftM` getEnv "PIVOTAL_TRACKER_API_TOKEN"
  runDbIO $ runMigrationUnsafe migrateAll
  port <- read `liftM` getEnv "PORT"
  let environment = Environment apiToken
  WS.scotty port $ do
    WS.post "/" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ labelStories label environment gitLog
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
