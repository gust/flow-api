{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}

import Schema
import qualified Web.Scotty as WS
import Control.Monad.Trans(liftIO)
import Data.Time.Clock(getCurrentTime)
import StringHelpers(lazyByteStringToString)
import PivotalTracker.Story
import PivotalTracker.Label(updateLabelsOnStories)
import Database.Persist
import Control.Monad.Trans.Reader( ReaderT(..))
import System.Environment(getEnv)
import Control.Monad(liftM)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger(runStdoutLoggingT)
import qualified Data.ByteString.Lazy as BL
import App.Environment
import World


connStr = "host=localhost dbname=flow_api user=gust port=5432"

runDbIO statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM statement connection

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
         runDbIO $ do
           pivotalIds <- insertMany pivotalStories
           time <- liftIO getCurrentTime
           releaseId <- insert $ Release time 
           let releaseStories = map (getReleaseStory releaseId) pivotalIds
           insertMany releaseStories
       WS.html "Success!"
