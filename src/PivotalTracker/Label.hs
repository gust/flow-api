{-# LANGUAGE OverloadedStrings    #-}
module PivotalTracker.Label(updateLabelsOnStories) where
  import Control.Monad.Trans(lift)
  import World
  import Schema
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

  extractByteString :: Value -> BCH.ByteString
  extractByteString (String t) = BCH.pack $ T.unpack t
  extractByteString  _         = ""

  extractInteger :: Value -> Integer
  extractInteger (Number s) = coefficient s
  extractInteger _ = -1

  pivotalLabelsFromReponse :: Value -> Maybe PivotalLabel
  pivotalLabelsFromReponse (Object val) = PivotalLabel <$> (extractInteger <$> HMS.lookup "id" val) <*> (extractByteString <$> HMS.lookup "name" val)

  labelsUrl :: PivotalStory -> String
  labelsUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (pivotalStoryProjectId story), "/stories/", (T.unpack $ pivotalStoryTrackerId story),  "/labels"]

  intToText = T.pack . show
