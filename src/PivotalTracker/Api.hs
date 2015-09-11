module PivotalTracker.Api(getStory) where
import Control.Monad.Trans.Reader(ReaderT, ask)
import PivotalTracker.Types(StoryId)
import World

getStory :: World m => StoryId -> ReaderT Environment m (Response a)
getStory storyId = do
  apiToken <- pivotalTrackerApiToken `liftM` ask
  let options = pivotalApiOptions apiToken
  res <- lift $ tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)
  case res of
    (Right response) -> return (pivotalStoryFromResponse response)
    Left (StatusCodeException status headers _) -> do
      case NHT.statusCode status of
        403 -> return Nothing
        404 -> return Nothing
        _   -> do
          lift . logError $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
          return Nothing
