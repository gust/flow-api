{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified DB.Schema as DB
import DB.DB(runDbIO, insertIfNew, keyFromParam)
import Network.Wai.Middleware.RequestLogger(logStdout)
import qualified Web.Scotty as WS
import App.SlackIntegration(notifyNewRelease)
import Control.Monad.Trans(liftIO)
import qualified Data.Aeson as DA
import qualified Control.Monad.State.Lazy as MS
import Data.Time.Clock(getCurrentTime)
import StringHelpers(lazyByteStringToString)
import Network.Wai.Middleware.Static
import Data.Monoid(mconcat)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT
import PivotalTracker.Story
import Control.Applicative((<$>))
import qualified Data.Text.Lazy as LT
import Control.Monad(forM)
import PivotalTracker.Label(updateLabelsOnStories)
import Control.Monad.Trans.Reader( ReaderT(..))
import Queries.Release(getReleases, getRelease)
import Control.Monad(liftM)
import qualified Data.ByteString.Char8 as BCH
import qualified Data.ByteString.Lazy as BL
import DB.Migration
import App.Environment
import World

labelStories :: World m => String -> Environment -> BL.ByteString -> m ()
labelStories label environment gitLog =
  flip runReaderT environment $ do
    stories <- fmap story `liftM` getStories gitLog
    updateLabelsOnStories label stories

getReleaseStory :: DB.ReleaseId -> DB.PivotalStoryId -> DB.ReleaseStory
getReleaseStory = DB.ReleaseStory

labelForApp app = "deployed to " ++ lazyByteStringToString app

doubleFmap f = fmap (fmap f)
type ReaderIO = ReaderT Environment IO ()

runReaderIO :: Environment -> ReaderIO -> IO ()
runReaderIO env r = runReaderT r env

main :: IO ()
main =  do
  environment <- loadEnvironment
  let connString = connectionString environment
  runMigrations connString
  WS.scotty (appPort environment) $ do
    WS.middleware $ staticPolicy (noDots >-> addBase "assets")
    WS.get "/" $ WS.file "index.html"

    WS.post "/deploys" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       liftIO $ labelStories (labelForApp app) environment gitLog
       WS.html . mconcat $ ["<h1>" , (T.pack $ lazyByteStringToString gitLog) , " </h1>" , "<h2>" , (T.pack $ lazyByteStringToString app) , "</h2>"]

    WS.get "/releases/:id" $ do
      id <- WS.param "id"
      release <- liftIO $ runDbIO connString (getRelease $ keyFromParam id)
      WS.json release

    WS.get "/releases" $ do
      releases <- liftIO $ runDbIO connString getReleases
      WS.json releases

    WS.post "/releases" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       liftIO $ flip runReaderT environment $ do
         stories <- getStories gitLog
         updateLabelsOnStories (labelForApp app) $ map story stories
         liftIO $ runDbIO connString $ do
           pivotalStoryIds <- mapM insertPivotalStory stories
           time <- liftIO getCurrentTime
           releaseId <- insert $ DB.Release time (Just . LT.toStrict . LT.pack $ lazyByteStringToString gitLog)
           let releaseStories = map (getReleaseStory releaseId) pivotalStoryIds
           liftIO . (notifyNewRelease $ slackEndpoint environment) . DT.pack . show . unSqlBackendKey $ DB.unReleaseKey releaseId
           mapM_ insertIfNew releaseStories
       WS.html "Success!"
