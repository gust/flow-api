{-# LANGUAGE OverloadedStrings    #-}

module PivotalTracker(getStories, lazyByteStringToString) where
import World
import Control.Monad.Trans(lift)
import Migrations
import Control.Monad.Trans.Reader
import Control.Lens((.~), (^.), (^?), (&), re)
import Control.Monad.Trans(liftIO)
import qualified Data.ByteString.Char8 as BCH
import Control.Monad(liftM, (>=>), liftM4)
import System.Environment(getEnv)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header)
import Data.Aeson.Lens (_String, key, _Integer, _Array, _Value)
import Data.Aeson(Value(..))
import Data.Scientific(coefficient, Scientific(..))
import qualified Network.HTTP.Types as NHT
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Applicative((<$>), (<*>))
import qualified Data.List as DL
import qualified Data.Maybe as MB
import qualified Data.HashMap.Strict as HMS
import qualified Text.Regex as TR
import qualified Data.Vector as V
import App.Environment
import qualified Network.Wreq as NW
import qualified Network.Wreq.Types as NWT

type StoryId = String
type CommitMessage = String

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

lazyByteStringToString = BCH.unpack . BL.toStrict

logError :: World m => String -> m a
logError = undefined

formNumber :: Value -> Scientific
formNumber (Number v) = v

getStory :: World m => StoryId -> ReaderT Environment m (Maybe PivotalStory)
getStory storyId = do
  apiToken <- pivotalTrackerApiToken `liftM` ask
  let options = pivotalApiOptions apiToken
  res <- lift $ tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)
  case res of
    (Right response) -> do
      let pivotalProjectId = response ^? responseBody . key "project_id"
      let pivotalStoryName = response ^? responseBody . key "name" . _String
      let pivotalStoryDescription = response ^? responseBody . key "description" . _String
      let pivotalStoryKind = response ^? responseBody . key "kind" . _String
      let id = Just $ T.pack storyId
      let convertedProjectId = (fromIntegral . coefficient . formNumber) <$> pivotalProjectId
      let requestedById = fromIntegral <$> response ^? responseBody . key "requested_by_id" . _Integer
      let storyType = response ^? responseBody . key "story_type" . _String
      let storyUrl = response ^? responseBody . key "url" . _String
      let owners = response ^? responseBody . key "owner_ids" . _Array 
      return $ PivotalStory <$> convertedProjectId <*> id <*> pivotalStoryName <*> pivotalStoryDescription <*> pivotalStoryKind <*> requestedById <*> storyType <*> storyUrl <*> owners
    Left (StatusCodeException status headers _) -> do
      case NHT.statusCode status of
        403 -> return Nothing
        404 -> return Nothing
        _   -> do
          lift . logError $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
          return Nothing
