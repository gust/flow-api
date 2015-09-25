{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs    #-}

import qualified Schema as DB
import Network.Wai.Middleware.RequestLogger(logStdout)
import GHC.Generics(Generic)
import Data.List(find)
import qualified Web.Scotty as WS
import           Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Trans(MonadIO, liftIO)
import qualified Data.Aeson as DA
import qualified Control.Monad.State.Lazy as MS
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Time.Clock(getCurrentTime)
import StringHelpers(lazyByteStringToString)
import Network.Wai.Middleware.Static
import Data.Monoid(mconcat)
import qualified Data.Text.Lazy as T
import PivotalTracker.Story
import Control.Applicative((<$>))
import qualified Data.Text.Lazy as LT
import Control.Monad(forM)
import PivotalTracker.Label(updateLabelsOnStories)
import Database.Persist
import Control.Monad.Trans.Reader( ReaderT(..))
import System.Environment(getEnv)
import Control.Monad(liftM, liftM5)
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BCH
import Control.Monad.Logger(runStdoutLoggingT, MonadLogger)
import qualified Data.ByteString.Lazy as BL
import App.Environment
import World


{- connStr = "host=localhost dbname=flow_api user=gust port=5432" -}

runDbIO :: BCH.ByteString -> SqlPersistM a -> IO a
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

doubleFmap f = fmap (fmap f)
type ReaderIO = ReaderT Environment IO ()

releaseDataFromSqlEntities :: [(Entity DB.Release, Entity DB.ReleaseStory, Entity DB.PivotalStory, Entity DB.PivotalStoryOwner, Entity DB.PivotalUser) ] -> [ReleaseData]
releaseDataFromSqlEntities xs = groupReleases $ fmap extractEntityValues xs
  where extractEntityValues (a, b, c, d, e) = (entityVal a, entityVal b, entityVal c, entityVal d, entityVal e)

getReleases ::  (MonadBaseControl IO m, MonadLogger m, MonadIO m) =>  SqlPersistT m [ReleaseData]
getReleases  = fmap releaseDataFromSqlEntities $ E.select $ E.from $ \(release `E.InnerJoin` releaseStory `E.InnerJoin`  pivotalStory `E.InnerJoin`  pivotalStoryOwner `E.InnerJoin` pivotalUser) -> do 
                  E.on(pivotalStoryOwner ^.  DB.PivotalStoryOwnerPivotalUserId E.==. pivotalUser ^. DB.PivotalUserId)
                  E.on(pivotalStoryOwner ^.  DB.PivotalStoryOwnerPivotalStoryId E.==. pivotalStory ^. DB.PivotalStoryId)
                  E.on(releaseStory ^.  DB.ReleaseStoryPivotalStoryId E.==. pivotalStory ^. DB.PivotalStoryId)
                  E.on(release ^. DB.ReleaseId E.==. releaseStory ^. DB.ReleaseStoryReleaseId)
                  return(release, releaseStory, pivotalStory, pivotalStoryOwner, pivotalUser)

data ReleaseData = ReleaseData { pivotalStories :: [PivotalStory], release :: DB.Release } deriving Generic
instance Eq ReleaseData where

instance DA.ToJSON ReleaseData

instance DA.ToJSON DB.Release
instance DA.ToJSON DB.PivotalStory
instance DA.ToJSON PivotalStory
instance DA.ToJSON DB.PivotalUser

groupReleases :: [(DB.Release, DB.ReleaseStory, DB.PivotalStory, DB.PivotalStoryOwner, DB.PivotalUser)] -> [ReleaseData]
groupReleases xs = flip MS.execState [] $ forM xs $ \releaseData@(release, _, st, _, user) -> do
                     releases <- MS.get
                     case findRelease release releases of
                        Just existingRelease  -> MS.put ((updateReleaseData existingRelease releaseData) : releases)
                        Nothing               -> MS.put ((newRelease releaseData) : releases)
  where
    updateReleaseData :: ReleaseData -> (DB.Release, DB.ReleaseStory, DB.PivotalStory, DB.PivotalStoryOwner, DB.PivotalUser) -> ReleaseData
    updateReleaseData releaseData (release, _, st, _, user) = ReleaseData (addStory releaseData st user) release
      where
        addStory :: ReleaseData -> DB.PivotalStory -> DB.PivotalUser -> [PivotalStory]
        addStory (ReleaseData stories _) st user = case find (\x -> (DB.pivotalStoryUrl $ story x) == (DB.pivotalStoryUrl st)) stories  of
                                                      Just existingStory ->  stories
                                                      Nothing             -> (PivotalStory st [user]) : stories

    newRelease :: (DB.Release, DB.ReleaseStory, DB.PivotalStory, DB.PivotalStoryOwner, DB.PivotalUser) -> ReleaseData
    newRelease (release, _, story, _, user) = ReleaseData [PivotalStory story [user]] release
    findRelease :: DB.Release -> [ReleaseData] -> Maybe ReleaseData
    findRelease r1 releases = flip find releases $ \releaseData -> do
                      DB.releaseCreatedAt (release releaseData) == DB.releaseCreatedAt r1


runReaderIO :: Environment -> ReaderIO -> IO ()
runReaderIO env r = runReaderT r env
main :: IO ()
main =  do
  apiToken <- BCH.pack `liftM` getEnv "PIVOTAL_TRACKER_API_TOKEN"
  connectionString <- liftM5 parsePostgresConnectionUrl (getEnv "DATABASE_HOST") (getEnv "DATABASE_NAME") (getEnv "DATABASE_USER") (getEnv "DATABASE_PASSWORD") (getEnv "DATABASE_PORT")
  runDbIO connectionString (runMigrationUnsafe DB.migrateAll)
  port <- read `liftM` getEnv "PORT"
  let environment = Environment apiToken
  WS.scotty port $ do
    WS.middleware $ staticPolicy (noDots >-> addBase "assets")
    WS.middleware logStdout
    WS.get "/" $ WS.file "index.html"
    WS.post "/deploys" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       liftIO $ BL.putStrLn gitLog
       liftIO $ BL.putStrLn app
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ labelStories label environment gitLog
       WS.html . mconcat $ ["<h1>" , (T.pack $ lazyByteStringToString gitLog) , " </h1>" , "<h2>" , (T.pack $ lazyByteStringToString app) , "</h2>"]
    WS.get "/releases" $ do
      releases <- liftIO $ runDbIO connectionString getReleases
      WS.json releases
    WS.post "/releases" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let label = "deployed to " ++ (lazyByteStringToString app)
       liftIO $ flip runReaderT environment $ do
         stories <- getStories gitLog
         updateLabelsOnStories label $ map story stories
         -- Why does this compile? Doesn't runStdoutLoggingT return IO??
         liftIO $ runDbIO connectionString$ do
           pivotalStoryIds <- mapM insertPivotalStory stories
           time <- liftIO getCurrentTime
           releaseId <- insert $ DB.Release time (Just . LT.toStrict . LT.pack $ lazyByteStringToString gitLog)
           let releaseStories = map (getReleaseStory releaseId) pivotalStoryIds
           mapM_ insertIfNew releaseStories
       WS.html "Success!"
