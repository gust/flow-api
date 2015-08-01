{-# LANGUAGE OverloadedStrings    #-}

module PivotalTracker where
import Control.Monad.Trans(lift)
import Migrations
import Control.Monad.Trans.Reader
import Control.Lens((.~), (^.), (^?), (&), re)
import qualified Data.ByteString.Char8 as BCH
import Control.Monad(liftM, (>=>))
import System.Environment(getEnv)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header, Response)
import Data.Aeson.Lens (_String, key, _Integer, _Array, _Value)
import Data.Aeson(Value(..))
import Data.Scientific(coefficient)
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
import Control.Exception as E

type StoryId = String
data PivotalLabel = PivotalLabel { labelId :: Integer, labelText :: BCH.ByteString  } deriving Show
type CommitMessage = String
type Label = String

instance World IO where
  getWith = NW.getWith
  postWith = NW.postWith
  deleteWith = NW.deleteWith
  tryRequest = E.try


class Monad m => World m where
  getWith :: Monad m => NW.Options -> String -> m (Response BL.ByteString)
  postWith :: (Monad m, NWT.Postable b) => NW.Options -> String -> b -> m (Response BL.ByteString)
  deleteWith :: Monad m => NW.Options -> String -> m (Response BL.ByteString)
  tryRequest :: Monad m => m a ->  m (Either HttpException a)


getStories :: World m => BL.ByteString -> ReaderT Environment m [PivotalStory]
getStories gitLog =  liftM MB.catMaybes $ pivotalStories . storyIdsFromCommits $ lines (lazyByteStringToString gitLog)

updateLabelsOnStories :: World m => String -> [PivotalStory] -> ReaderT Environment m ()
updateLabelsOnStories label stories = mapM_ (updateLabels label) stories

getApiToken :: IO BCH.ByteString
getApiToken = liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"

labelsUrl :: PivotalStory -> String
labelsUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (pivotalStoryProjectId story), "/stories/", (T.unpack $ pivotalStoryTrackerId story),  "/labels"]

extractText :: Value -> BCH.ByteString
extractText (String t) = BCH.pack $ T.unpack t
extractText  _         = ""

extractInteger :: Value -> Integer
extractInteger (Number s) = coefficient s
extractInteger _ = -1

pivotalLabelsFromReponse :: Value -> Maybe PivotalLabel
pivotalLabelsFromReponse (Object val) = PivotalLabel <$> (extractInteger <$> HMS.lookup "id" val) <*> (extractText <$> HMS.lookup "name" val)

emptyBody = "" :: BCH.ByteString

intToText = T.pack . show
labelDestroyUrl :: PivotalStory -> PivotalLabel ->  T.Text
labelDestroyUrl story label = T.concat ["https://www.pivotaltracker.com/services/v5/projects/", intToText (pivotalStoryProjectId story), "/stories/", (pivotalStoryTrackerId story), "/labels/", (T.pack . show $ labelId label) ]

getPreviousDeployLabels :: World m => PivotalStory -> ReaderT Environment m [PivotalLabel]
getPreviousDeployLabels story = do
    apiToken <- pivotalTrackerApiToken `liftM` ask 
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    response <- lift . tryRequest $ getWith requestOptions (labelsUrl story)
    case response of
      Right res -> do
        let stories = MB.catMaybes  . V.toList $ V.map pivotalLabelsFromReponse (res ^. responseBody . _Array)
        return stories
      Left  a   -> return []


pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token]

lazyByteStringToString = BCH.unpack . BL.toStrict

storyIdsFromCommits :: [CommitMessage] -> [StoryId]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId)
  where
    parseStoryId :: CommitMessage -> Maybe [StoryId]
    parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")


pivotalStories :: World m => [StoryId] -> ReaderT Environment m [Maybe PivotalStory]
pivotalStories storyIds = mapM getStory storyIds where

logError :: World m => String -> m a
logError = undefined

getStory :: World m => StoryId -> ReaderT Environment m (Maybe PivotalStory)
getStory storyId = do
  apiToken <- pivotalTrackerApiToken `liftM` ask 
  let options = pivotalApiOptions apiToken
  res <- lift $ tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)
  case res of
    (Right response) -> do
      let pivotalProjectId = response ^? responseBody . key "project_id"
      case pivotalProjectId of
        Just (Number projectId) -> return . Just $ PivotalStory (fromIntegral $ coefficient projectId) $ T.pack storyId
        Nothing    -> return Nothing
    Left (StatusCodeException status headers _) -> do
      case NHT.statusCode status of
        403 -> return Nothing
        404 -> return Nothing
        _   -> do
          lift . logError $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
          return Nothing


removeLabels :: World m => PivotalStory -> [PivotalLabel] -> ReaderT Environment m ()
removeLabels story labels = do
  mapM_ removeLabel labels
  where
    removeLabel :: World m => PivotalLabel -> ReaderT Environment m ()
    removeLabel label = do
      apiToken <- pivotalTrackerApiToken `liftM` ask 
      let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
      lift . tryRequest . (deleteWith requestOptions) . T.unpack $ labelDestroyUrl story label
      return ()

partOfDeployPipeline :: PivotalLabel -> Bool
partOfDeployPipeline label = labelText label `elem` deploymentPipelineLabels

deploymentPipelineLabels =  ["deployed to zephyr-production", "deployed to zephyr-integration", "deployed to zephyr-preproduction" ]

deploymentLabel :: PivotalLabel -> Bool
deploymentLabel label = "deployed to " `BCH.isInfixOf` labelText label


newLabelNeeded :: Label -> [PivotalLabel] -> Bool
newLabelNeeded newLabel previousLabels = (deploymentPipelineLabel newLabel) || (not $ any inPipeline previousLabels)
  where
    deploymentPipelineLabel :: Label ->  Bool
    deploymentPipelineLabel newLabel = (BCH.pack newLabel) `elem` deploymentPipelineLabels
    inPipeline :: PivotalLabel -> Bool
    inPipeline label = labelText label `elem` deploymentPipelineLabels


updateLabels :: World m => Label -> PivotalStory -> ReaderT Environment m ()
updateLabels label story = do
  previousLabels <- getPreviousDeployLabels story
  if newLabelNeeded label previousLabels
  then do
    let labelsToDestroy = filter (not . partOfDeployPipeline) $ filter deploymentLabel previousLabels
    removeLabels story labelsToDestroy
    labelStory label story
  else return ()

labelStory :: World m => Label -> PivotalStory -> ReaderT Environment m ()
labelStory label story = do
    apiToken <- pivotalTrackerApiToken `liftM` ask 
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    let formBody = "name" := label
    lift $ tryRequest $ postWith requestOptions (labelsUrl story) formBody 
    return ()

