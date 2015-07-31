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

module Migrations where
import Database.Persist.TH
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Release
  title T.Text
  createdAt UTCTime
  deriving Show

ReleaseAuthor
  releaseId ReleaseId
  userId UserId
  deriving Show

ReleaseStory
  releaseId ReleaseId
  pivotalStoryId PivotalStoryId
  commitSha T.Text
  UniqueReleaseStoryCommit releaseId pivotalStoryId commitSha
  deriving Show

PivotalStory
  projectId Int
  trackerId T.Text
  deriving Show

User
  firstName T.Text
  lastName T.Text
  deriving Show
|]

