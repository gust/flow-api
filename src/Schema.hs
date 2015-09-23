{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Schema where
import Database.Persist.TH
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Release
  createdAt UTCTime
  gitLog T.Text Maybe
  deriving Show

ReleaseStory
  releaseId ReleaseId
  pivotalStoryId PivotalStoryId
  UniqueReleaseStoryCommit releaseId pivotalStoryId
  deriving Show

PivotalStory
  currentState T.Text
  estimate Int
  projectId Int
  trackerId T.Text
  name T.Text
  description T.Text
  kind T.Text
  requestedById Int
  storyType T.Text
  url T.Text
  UniquePivotalStory trackerId
  deriving Show

PivotalStoryOwner
  pivotalUserId PivotalUserId
  pivotalStoryId PivotalStoryId
  UniqueStoryOwner pivotalUserId pivotalStoryId
  deriving Show

PivotalUser
  pivotalId Int
  UniqueUserId pivotalId
  deriving Show
|]




