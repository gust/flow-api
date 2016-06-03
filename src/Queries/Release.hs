{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric #-}

module Queries.Release(getReleases, getRelease, releaseCount) where

import GHC.Generics(Generic)
import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Logger(runStdoutLoggingT, MonadLogger)
import Data.Int(Int64(..))
import qualified Schema as DB
import Control.Monad.Trans(MonadIO)
import PivotalTracker.Story(PivotalStory(..))
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.), (?.))
import Database.Persist(SelectOpt(..),selectKeysList, count ,Entity, entityVal, Key(..), Filter)
import qualified Data.Aeson as DA
import Database.Persist.Postgresql(SqlPersistT)
import Control.Monad(forM)
import Pagination(Pagination(..), pOffset)
import Data.List(find)
import qualified Control.Monad.State.Lazy as MS

data ReleaseData = ReleaseData { pivotalStories :: [PivotalStory], release :: DB.Release } deriving Generic

instance Eq ReleaseData where
  (==) x y = (release x) == (release y)
  (/=) x y = (release x) /= (release y)
instance DA.ToJSON DB.Release
instance DA.ToJSON DB.PivotalStory
instance DA.ToJSON PivotalStory
instance DA.ToJSON DB.PivotalUser

instance DA.ToJSON ReleaseData
getRelease ::  (MonadBaseControl IO m, MonadLogger m, MonadIO m) => DB.ReleaseId -> SqlPersistT m [ReleaseData]
getRelease releaseId = fmap releaseDataFromSqlEntities $ E.select $ E.from $ \(release `E.LeftOuterJoin` releaseStory `E.LeftOuterJoin`  pivotalStory) -> do 
                  E.on(releaseStory ?.  DB.ReleaseStoryPivotalStoryId E.==. pivotalStory ?. DB.PivotalStoryId)
                  E.on(E.just(release ^. DB.ReleaseId) E.==. releaseStory ?. DB.ReleaseStoryReleaseId)
                  E.where_ (release ^. DB.ReleaseId E.==. E.val releaseId)
                  return(release, releaseStory, pivotalStory)

getReleases ::  (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Pagination ->  SqlPersistT m [ReleaseData]
getReleases pagination = do
          releaseIds <- selectKeysList [] [ LimitTo (perPage pagination), OffsetBy (pOffset pagination), Desc DB.ReleaseCreatedAt] :: MonadIO m => SqlPersistT m [DB.ReleaseId]
          fmap releaseDataFromSqlEntities $  E.select $ E.from $ \(release `E.LeftOuterJoin` releaseStory `E.LeftOuterJoin`  pivotalStory) -> do 
                E.on(releaseStory ?.  DB.ReleaseStoryPivotalStoryId E.==. pivotalStory ?. DB.PivotalStoryId)
                E.on((E.just $ release ^. DB.ReleaseId) E.==. releaseStory ?. DB.ReleaseStoryReleaseId)
                E.where_ (release ^. DB.ReleaseId `E.in_` E.valList releaseIds)
                return(release, releaseStory, pivotalStory)


releaseCount :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => SqlPersistT m Int
releaseCount = count ([] :: [Filter DB.Release])

releaseDataFromSqlEntities :: [(Entity DB.Release, Maybe (Entity DB.ReleaseStory), Maybe (Entity DB.PivotalStory)) ] -> [ReleaseData]
releaseDataFromSqlEntities xs = groupReleases $ fmap extractEntityValues xs
  where extractEntityValues (a, b, c) = (entityVal a, entityVal <$> b, entityVal <$> c)

--This method is probably poorly performant. We could use Data.Map instead of an array for storage here
--Which should perform better
groupReleases :: [(DB.Release, Maybe DB.ReleaseStory, Maybe  DB.PivotalStory)] -> [ReleaseData]
groupReleases xs = flip MS.execState [] $ forM xs $ \releaseData@(release, _, st) -> do
                     releases <- MS.get
                     case findRelease release releases of
                        Just existingRelease  -> MS.put ((updateReleaseData existingRelease releaseData) : (filterExistingRelease existingRelease releases))
                        Nothing               -> MS.put ((newRelease releaseData) : releases)
  where
    filterExistingRelease :: ReleaseData -> [ReleaseData] -> [ReleaseData]
    filterExistingRelease releaseData = filter (/= releaseData)

    updateReleaseData :: ReleaseData -> (DB.Release, Maybe DB.ReleaseStory, Maybe DB.PivotalStory) -> ReleaseData
    updateReleaseData releaseData (release, _, st) = ReleaseData (addStory releaseData st) release
      where
        addStory :: ReleaseData -> Maybe DB.PivotalStory -> [PivotalStory]
        addStory _ Nothing =  []
        addStory (ReleaseData stories _) (Just st) = case find (\x -> (DB.pivotalStoryUrl $ story x) == (DB.pivotalStoryUrl st)) stories  of
                                                      Just existingStory ->  stories
                                                      Nothing             -> (PivotalStory st []) : stories

    newRelease :: (DB.Release, Maybe DB.ReleaseStory, Maybe DB.PivotalStory) -> ReleaseData
    newRelease (release, _, Nothing)      = ReleaseData [] release
    newRelease (release, _, (Just story)) = ReleaseData [PivotalStory story []] release

    findRelease :: DB.Release -> [ReleaseData] -> Maybe ReleaseData
    findRelease r1 releases = flip find releases $ \releaseData -> do
                      DB.releaseCreatedAt (release releaseData) == DB.releaseCreatedAt r1
