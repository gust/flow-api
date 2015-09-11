{-# LANGUAGE OverloadedStrings    #-}

module PivotalTracker.Story(getStories, PivotalStory(..)) where

import App.Environment
import Control.Applicative((<$>), (<*>))
import Control.Lens((.~), (^.), (^?), (&), re, traverse)
import Control.Monad(liftM, (>=>), liftM4, join)
import Control.Monad.Trans(lift)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson(Value(..))
import Data.Aeson.Lens (_String, key, _Integer, _Array, _Value)
import Data.Scientific(coefficient, Scientific(..))
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header)
import PivotalTracker.Api(getStory)
import StringHelpers(lazyByteStringToString)
import System.Environment(getEnv)
import PivotalTracker.Types(StoryId)
import World
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMS
import qualified Data.List as DL
import qualified Data.Maybe as MB
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Types as NHT
import qualified Network.Wreq as NW
import qualified Network.Wreq.Types as NWT
import qualified Schema as DB
import qualified Text.Regex as TR

type CommitMessage = String

data PivotalStory = PivotalStory  { story :: DB.PivotalStory, owners :: [DB.PivotalUser] }

getStories :: World m => BL.ByteString -> ReaderT Environment m [PivotalStory]
getStories gitLog =  liftM MB.catMaybes $ pivotalStories . storyIdsFromCommits $ lines (lazyByteStringToString gitLog)

storyIdsFromCommits :: [CommitMessage] -> [StoryId]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId)
  where
    parseStoryId :: CommitMessage -> Maybe [StoryId]
    parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")

pivotalStories :: World m => [StoryId] -> ReaderT Environment m [Maybe PivotalStory]
pivotalStories storyIds = mapM getStory storyIds where

pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token]


logError :: World m => String -> m a
logError = undefined

formNumber :: Value -> Scientific
formNumber (Number v) = v

extractInteger :: Value -> Integer
extractInteger (Number s) = coefficient s
extractInteger _ = undefined

extractPivotalUsers :: V.Vector Value ->  Maybe [DB.PivotalUser]
extractPivotalUsers v =  Just $ fmap (DB.PivotalUser . fromIntegral . extractInteger) (V.toList v)

type PivotalApiResponse = NW.Response

pivotalStoryFromResponse :: PivotalApiResponse a -> Maybe PivotalStory
pivotalStoryFromResponse = undefined

