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

import qualified Web.Scotty as WS
import Data.String
import Data.Monoid
import Control.Monad.Trans
import qualified Data.HashMap.Strict as HMS
import Data.Scientific(coefficient)
import Control.Applicative((<$>), (<*>))
import Control.Monad(liftM, (>=>))
import Control.Monad.Trans
import TrackerTagging
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL
import qualified Data.ByteString.Char8 as BCH
import qualified Data.Maybe as MB
import Control.Applicative((<$>))
import qualified Data.Text as T
import qualified Text.Regex as TR
import System.Environment(getEnv)
import Network.Wreq(FormParam( (:=) ), deleteWith, postWith, defaults, getWith, responseBody, header)
import qualified Data.Vector as V
import Data.Scientific(coefficient)
import Data.Aeson(Value(..))
import Data.Aeson.Lens (_String, key, _Integer, _Array, _Value)
import Control.Lens((.~), (^.), (^?), (&), re)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import qualified Network.HTTP.Types as NHT
import Control.Exception as E
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

type CommitMessage = String
type StoryId = String
type Label = String

lazyByteStringToString = BCH.unpack . BL.toStrict
lazyByteStringToText   = T.pack . lazyByteStringToString

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

connStr = "host=localhost dbname=flow_api user=gust port=5432"
-- runSqlConn :: MonadBaseControl IO m => SqlPersistT m a -> SqlBackend -> m a
-- withSqlConn :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => (LogFunc -> IO SqlBackend) -> (SqlBackend -> m a) -> m a
-- withPostgresqlConn :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => ConnectionString -> (SqlBackend -> m a) -> m a

createReleaseStory :: [PivotalStory] -> IO ()
createReleaseStory = undefined


getStories :: BL.ByteString -> IO [PivotalStory]
getStories gitLog =  liftM MB.catMaybes $ pivotalStories . storyIdsFromCommits $ lines (lazyByteStringToString gitLog)

updateLabelsOnStories :: Label -> [PivotalStory] -> IO ()
updateLabelsOnStories label stories = do
  mapM_ (updateLabels label) stories

getApiToken :: IO BCH.ByteString
getApiToken = liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"

pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token]


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

tryRequest :: IO a ->  IO (Either HttpException a)
tryRequest = E.try

storyIdsFromCommits :: [CommitMessage] -> [StoryId]
storyIdsFromCommits = DL.nub . concat . (MB.mapMaybe parseStoryId)
  where
    parseStoryId :: CommitMessage -> Maybe [StoryId]
    parseStoryId = TR.matchRegex (TR.mkRegex "#([0-9]*)")

data PivotalLabel = PivotalLabel { labelId :: Integer, labelText :: BCH.ByteString  } deriving Show

labelsUrl :: PivotalStory -> String
labelsUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (pivotalStoryProjectId story), "/stories/", (T.unpack $ pivotalStoryTrackerId story),  "/labels"]

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

main :: IO ()
main =  do
  runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do 
     liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAll
  {- port <- liftM read $ getEnv "PORT" -}
  {- WS.scotty port $ do -}
    {- WS.post "/" $ do -}
       {- gitLog <- WS.param "git_log" -}
       {- app    <- WS.param "app" -}
       {- let label = "deployed to " ++ (lazyByteStringToString app) -}
       {- (liftIO $ (getStories gitLog) >>= (updateLabelsOnStories label)) >> (WS.html "<h1>success</h1>") -}
    {- WS.post "/releases" $ do -}
       {- gitLog <- WS.param "git_log" -}
       {- app    <- WS.param "app" -}
       {- let label = "deployed to " ++ (lazyByteStringToString app) -}
       {- pivotalStories <- liftIO $ getStories gitLog -}
       {- (liftIO $ updateLabelsOnStories label pivotalStories) >> WS.html "<h1>success</h1>" -}
       {- liftIO $ createReleaseStory pivotalStories -}


