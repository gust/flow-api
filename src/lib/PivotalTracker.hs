module PivotalTracker where
  import World
  import Control.Monad.Trans(lift, liftIO)
  import GHC.Generics(Generic)
  import qualified Schema as DB
  import Control.Monad.Trans.Reader
  import Control.Lens((.~), (^.), (^?), (&), re, traverse)
  import Control.Monad(liftM, (>=>), liftM4, join)
  import System.Environment(getEnv)
  import Network.HTTP.Conduit(HttpException(StatusCodeException) )
  import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header)
  import Data.Aeson.Lens (_String, key, _Integer, _Array, _Value)
  import Data.Aeson(Value(..))
  import Data.Scientific(coefficient, Scientific(..))
  import qualified Network.HTTP.Types as NHT
  import qualified Data.Text as T
  import qualified Data.ByteString.Lazy as BL
  import StringHelpers(lazyByteStringToString)
  import Control.Applicative((<$>), (<*>))
  import qualified Data.List as DL
  import qualified Data.Maybe as MB
  import qualified Data.HashMap.Strict as HMS
  import qualified Text.Regex as TR
  import qualified Data.Vector as V
  import App.Environment
  import qualified Network.Wreq as NW
  import qualified Network.Wreq.Types as NWT

  import Control.Monad.Trans.Reader
  import Control.Lens((.~), (^.), (^?), (&), re)
  import qualified Data.ByteString.Char8 as BCH
  import Control.Monad(liftM)
  import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header)
  import Data.Aeson.Lens (_Array)
  import Data.Aeson(Value(..))
  import Data.Scientific(coefficient, Scientific(..))
  import qualified Data.Text as T
  import Control.Applicative((<$>), (<*>))
  import qualified Data.Maybe as MB
  import qualified Data.HashMap.Strict as HMS
  import qualified Data.Vector as V
  import App.Environment
  import qualified Network.Wreq as NW
  import qualified Network.Wreq.Types as NWT

  updateLabelsOnStories :: World m => String -> [PivotalStory] -> ReaderT Environment m ()
  updateLabelsOnStories label stories = mapM_ (updateLabels label) stories

  data PivotalLabel = PivotalLabel { labelId :: Integer, labelText :: BCH.ByteString  } deriving Show
  type Label = String

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
  pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token]

  partOfDeployPipeline :: PivotalLabel -> Bool
  partOfDeployPipeline label = labelText label `elem` deploymentPipelineLabels


  labelDestroyUrl :: PivotalStory -> PivotalLabel ->  T.Text
  labelDestroyUrl story label = T.concat ["https://www.pivotaltracker.com/services/v5/projects/", intToText (DB.pivotalStoryProjectId story), "/stories/", (DB.pivotalStoryTrackerId story), "/labels/", (T.pack . show $ labelId label) ]

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

  extractByteString :: Value -> BCH.ByteString
  extractByteString (String t) = BCH.pack $ T.unpack t
  extractByteString  _         = ""

  extractInteger :: Value -> Integer
  extractInteger (Number s) = coefficient s
  extractInteger _ = -1

  pivotalLabelsFromReponse :: Value -> Maybe PivotalLabel
  pivotalLabelsFromReponse (Object val) = PivotalLabel <$> (extractInteger <$> HMS.lookup "id" val) <*> (extractByteString <$> HMS.lookup "name" val)

  labelsUrl :: PivotalStory -> String
  labelsUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (DB.pivotalStoryProjectId story), "/stories/", (T.unpack $ DB.pivotalStoryTrackerId story),  "/labels"]

  intToText = T.pack . show
  type StoryId = String
  type CommitMessage = String

  data PivotalStory = PivotalStory  { story :: DB.PivotalStory, owners :: [DB.PivotalUser] } deriving Generic

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
        let convertedProjectId = (fromIntegral . extractInteger) <$> pivotalProjectId
        let requestedById = fromIntegral <$> response ^? responseBody . key "requested_by_id" . _Integer
        let storyType = response ^? responseBody . key "story_type" . _String
        let storyUrl = response ^? responseBody . key "url" . _String
        let owners = extractPivotalUsers $ response ^. responseBody . key "owner_ids" . _Array
        let currentState = response ^? responseBody . key "current_state" . _String
        let estimate = response ^? responseBody . key "estimate"  . _Integer
        let storyFromJSON = DB.PivotalStory <$> currentState <*> (fromIntegral <$> estimate) <*> convertedProjectId <*> id <*> pivotalStoryName <*> pivotalStoryDescription <*> pivotalStoryKind <*> requestedById <*> storyType <*> storyUrl
        return $ PivotalStory <$> storyFromJSON <*> owners
      Left (StatusCodeException status headers _) -> do
        case NHT.statusCode status of
          403 -> return Nothing
          404 -> return Nothing
          _   -> do
            {- lift . logError $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted" -}
            return Nothing
