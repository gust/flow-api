{-# LANGUAGE OverloadedStrings    #-}

module PivotalTracker where
import Migrations
import Control.Lens((.~), (^.), (^?), (&), re)
import qualified Data.ByteString.Char8 as BCH
import Control.Monad(liftM, (>=>))
import System.Environment(getEnv)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import Control.Exception as E
import Network.Wreq(FormParam( (:=) ), deleteWith, postWith, defaults, getWith, responseBody, header)
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

type StoryId = String
data PivotalLabel = PivotalLabel { labelId :: Integer, labelText :: BCH.ByteString  } deriving Show
type CommitMessage = String
type Label = String

lazyByteStringToString = BCH.unpack . BL.toStrict

tryRequest :: IO a ->  IO (Either HttpException a)
tryRequest = E.try

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

getPreviousDeployLabels :: PivotalStory -> IO [PivotalLabel]
getPreviousDeployLabels story = do
    apiToken <- getApiToken
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    response <- tryRequest $ getWith requestOptions (labelsUrl story)
    case response of
      Right res -> do
        let stories = MB.catMaybes  . V.toList $ V.map pivotalLabelsFromReponse (res ^. responseBody . _Array)
        return stories
      Left  a   -> return []


pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token]
getApiToken :: IO BCH.ByteString
getApiToken = liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"

getStories :: BL.ByteString -> IO [PivotalStory]
getStories gitLog =  liftM MB.catMaybes $ pivotalStories . storyIdsFromCommits $ lines (lazyByteStringToString gitLog)

storyIdsFromCommits :: [CommitMessage] -> [StoryId]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId)
  where
    parseStoryId :: CommitMessage -> Maybe [StoryId]
    parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")

updateLabelsOnStories :: Label -> [PivotalStory] -> IO ()
updateLabelsOnStories label stories = do
  mapM_ (updateLabels label) stories


pivotalStories :: [StoryId] -> IO [Maybe PivotalStory]
pivotalStories storyIds = mapM getStory storyIds where
  getStory :: StoryId -> IO (Maybe PivotalStory)
  getStory storyId = do
    apiToken <- getApiToken
    let options = pivotalApiOptions apiToken
    res <- tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId)

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
            putStrLn $ "Could not process request for story: " ++ storyId ++ " defaulting to not accepted"
            return Nothing


removeLabels :: PivotalStory -> [PivotalLabel] -> IO ()
removeLabels story labels = do
  mapM_ removeLabel labels
  where
    removeLabel label = do
      apiToken <- getApiToken
      let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
      tryRequest $ deleteWith requestOptions $ T.unpack $ labelDestroyUrl story label

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


updateLabels :: Label -> PivotalStory -> IO ()
updateLabels label story = do
  previousLabels <- getPreviousDeployLabels story
  if newLabelNeeded label previousLabels
  then do
    let labelsToDestroy = filter (not . partOfDeployPipeline) $ filter deploymentLabel previousLabels
    removeLabels story labelsToDestroy
    labelStory label story
  else return ()

labelStory :: Label -> PivotalStory -> IO ()
labelStory label story = do
    apiToken <- getApiToken
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    let formBody = "name" := label
    (tryRequest $ postWith requestOptions (labelsUrl story) formBody) >> return ()

