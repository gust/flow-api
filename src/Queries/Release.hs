{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric #-}

module Queries.Release(getReleases) where

import GHC.Generics(Generic)
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Logger(runStdoutLoggingT, MonadLogger)
import qualified Schema as DB
import Control.Monad.Trans(MonadIO)
import PivotalTracker.Story(PivotalStory(..))
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist(Entity, entityVal)
import qualified Data.Aeson as DA
import Database.Persist.Postgresql(SqlPersistT)
import Control.Monad(forM)
import Data.List(find)
import qualified Control.Monad.State.Lazy as MS

data ReleaseData = ReleaseData { pivotalStories :: [PivotalStory], release :: DB.Release } deriving Generic
instance DA.ToJSON DB.Release
instance DA.ToJSON DB.PivotalStory
instance DA.ToJSON PivotalStory
instance DA.ToJSON DB.PivotalUser

instance DA.ToJSON ReleaseData

getReleases ::  (MonadBaseControl IO m, MonadLogger m, MonadIO m) =>  SqlPersistT m [ReleaseData]
getReleases  = fmap releaseDataFromSqlEntities $ E.select $ E.from $ \(release `E.InnerJoin` releaseStory `E.InnerJoin`  pivotalStory `E.InnerJoin`  pivotalStoryOwner `E.InnerJoin` pivotalUser) -> do 
                  E.on(pivotalStoryOwner ^.  DB.PivotalStoryOwnerPivotalUserId E.==. pivotalUser ^. DB.PivotalUserId)
                  E.on(pivotalStoryOwner ^.  DB.PivotalStoryOwnerPivotalStoryId E.==. pivotalStory ^. DB.PivotalStoryId)
                  E.on(releaseStory ^.  DB.ReleaseStoryPivotalStoryId E.==. pivotalStory ^. DB.PivotalStoryId)
                  E.on(release ^. DB.ReleaseId E.==. releaseStory ^. DB.ReleaseStoryReleaseId)
                  return(release, releaseStory, pivotalStory, pivotalStoryOwner, pivotalUser)


releaseDataFromSqlEntities :: [(Entity DB.Release, Entity DB.ReleaseStory, Entity DB.PivotalStory, Entity DB.PivotalStoryOwner, Entity DB.PivotalUser) ] -> [ReleaseData]
releaseDataFromSqlEntities xs = groupReleases $ fmap extractEntityValues xs
  where extractEntityValues (a, b, c, d, e) = (entityVal a, entityVal b, entityVal c, entityVal d, entityVal e)

--This method is probably poorly performant. We could use Data.Map instead of an array for storage here
--Which should perform better
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
