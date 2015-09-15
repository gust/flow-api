{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs    #-}

import qualified Schema as DB
import Network.Wai.Middleware.RequestLogger(logStdout)
import qualified Web.Scotty as WS
import Control.Monad.Trans(MonadIO, liftIO)
import Data.Time.Clock(getCurrentTime)
import StringHelpers(lazyByteStringToString)
import PivotalTracker.Story
import Control.Applicative((<$>))
import PivotalTracker.Label(updateLabelsOnStories)
import Database.Persist
import Control.Monad.Trans.Reader( ReaderT(..))
import System.Environment(getEnv)
import Control.Monad(liftM, liftM5)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger(runStdoutLoggingT)
import qualified Data.ByteString.Lazy as BL
import App.Environment
import World


{- connStr = "host=localhost dbname=flow_api user=gust port=5432" -}

runDbIO connStr statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection -> do
          liftIO $ runSqlPersistM statement connection

labelStories :: World m => String -> Environment -> BL.ByteString -> m ()
labelStories label environment gitLog = do
  flip runReaderT environment $ do
    stories <- (fmap story) `liftM` getStories gitLog
    updateLabelsOnStories label stories

getReleaseStory :: DB.ReleaseId -> DB.PivotalStoryId -> DB.ReleaseStory
getReleaseStory releaseId storyId = DB.ReleaseStory releaseId storyId

insertPivotalStory (PivotalStory story pivotalUsers) = do 
  pivotalUsers <- mapM insertIfNew pivotalUsers
  let ownerIds = fmap entityKey pivotalUsers
  storyId <- entityKey <$> insertIfNew story
  pivotalStoryOwnerIds <- mapM insertIfNew $ map (flip DB.PivotalStoryOwner storyId) ownerIds
  return storyId

insertIfNew :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val, PersistUnique backend) => val -> ReaderT backend m (Entity val)
insertIfNew = flip upsert []

parsePostgresConnectionUrl :: String -> String -> String -> String -> String -> BCH.ByteString
parsePostgresConnectionUrl host dbname user password port = BCH.pack $ "host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++ " password=" ++ password ++ " port=" ++ port

type ReaderIO = ReaderT Environment IO ()
runReaderIO :: Environment -> ReaderIO -> IO ()
runReaderIO env r = runReaderT r env
main :: IO ()
main =  do
  apiToken <- BCH.pack `liftM` getEnv "PIVOTAL_TRACKER_API_TOKEN"
  connectionString <- liftM5 parsePostgresConnectionUrl (getEnv "DATABASE_HOST") (getEnv "DATABASE_NAME") (getEnv "DATABASE_USER") (getEnv "DATABASE_PASSWORD") (getEnv "DATABASE_PORT")
  let runDb = runDbIO connectionString
  runDb (runMigrationUnsafe DB.migrateAll)
  port <- read `liftM` getEnv "PORT"
  let environment = Environment apiToken
  WS.scotty port $ do
    WS.middleware logStdout

    WS.post "/" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       liftIO $ BL.putStrLn gitLog
       liftIO $ BL.putStrLn app
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ labelStories label environment gitLog
       WS.html "<h1>success</h1>"
    WS.post "/releases" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ flip runReaderT environment $ do
         stories <- getStories gitLog
         updateLabelsOnStories label $ map story stories
         -- Why does this compile? Doesn't runStdoutLoggingT return IO??
         liftIO $ runDb $ do
           pivotalStoryIds <- mapM insertPivotalStory stories
           time <- liftIO getCurrentTime
           releaseId <- insert $ DB.Release time
           let releaseStories = map (getReleaseStory releaseId) pivotalStoryIds
           mapM_ insertIfNew releaseStories
       WS.html "Success!"
