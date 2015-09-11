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
  deriving Show

ReleaseStory
  releaseId ReleaseId
  pivotalStoryId PivotalStoryId
  UniqueReleaseStoryCommit releaseId pivotalStoryId
  deriving Show

PivotalStory
  projectId Int
  trackerId T.Text
  name T.Text
  description T.Text
  kind T.Text
  requested_by_id Int
  story_type T.Text
  url T.Text
  owners [PivotalUser]
  deriving Show

PivotalStoryOwner
  pivotalUserId PivotalUserId
  pivotalStoryId PivotalStoryId
  deriving Show

PivotalUser
  pivotalId Int
  deriving Show
|]

