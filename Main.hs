{-# LANGUAGE OverloadedStrings    #-}

import Migrations
import qualified Web.Scotty as WS
import Control.Monad.Trans
import Control.Monad.State.Class
import Data.Time.Clock(getCurrentTime)
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

getReleaseStory :: ReleaseId -> PivotalStoryId -> ReleaseStory
getReleaseStory releaseId storyId = ReleaseStory releaseId storyId

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
    WS.post "/releases" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ flip runReaderT environment $ do
         pivotalStories <- getStories gitLog
         updateLabelsOnStories label pivotalStories
         -- Why does this compile? Doesn't runStdoutLoggingT return IO??
         runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
            liftIO $ flip runSqlPersistM connection $ do
              pivotalIds <- insertMany pivotalStories
              time <- liftIO getCurrentTime
              releaseId <- insert $ Release time 
              let releaseStories = map (getReleaseStory releaseId) pivotalIds
              insertMany releaseStories

       WS.html "Success!"
