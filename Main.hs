{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Scotty as WS
import Data.String
import Data.Monoid
import Control.Monad(liftM)
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
import Network.Wreq(FormParam( (:=) ), postWith, defaults, getWith, responseBody, header)
import Data.Scientific(coefficient)
import Data.Aeson(Value(..))
import Data.Aeson.Lens (_String, key, _Integer)
import Control.Lens((.~), (^.), (^?), (&), re)
import Network.HTTP.Conduit(HttpException(StatusCodeException) )
import qualified Network.HTTP.Types as NHT
import Control.Exception as E

type CommitMessage = String
type StoryId = String
type Tag = String
data PivotalStory = PivotalStory { projectId ::  Integer, storyId :: T.Text } deriving Show

lazyByteStringToString = BCH.unpack . BL.toStrict 

main =  do 
  port <- liftM read $ getEnv "FLOW_API_PORT"
  WS.scotty port $ do
    WS.post "/" $ do
       gitLog <- WS.param "git_log"
       app    <- WS.param "app"
       let tag = "deployed to " ++ (read $ lazyByteStringToString app)
       (liftIO $ (getStories gitLog) >>= (tagStories tag)) >> (WS.html "<h1>success</h1>")

getStories :: BL.ByteString -> IO [PivotalStory]
getStories gitLog =  liftM MB.catMaybes $ pivotalStories . storyIdsFromCommits $ lines (lazyByteStringToString gitLog)

tagStories :: Tag -> [PivotalStory] -> IO ()
tagStories tag = mapM_ (tagStory tag)

getApiToken :: IO BCH.ByteString
getApiToken = liftM BCH.pack $ getEnv "PIVOTAL_TRACKER_API_TOKEN"

pivotalApiOptions token = defaults & header "X-TrackerToken" .~ [token] 


pivotalStories :: [StoryId] -> IO [Maybe PivotalStory]
pivotalStories storyIds = mapM getStory storyIds where
  getStory :: StoryId -> IO (Maybe PivotalStory)
  getStory storyId = do 
    apiToken <- getApiToken
    print $ "Getting Story with Token " ++ BCH.unpack apiToken
    let options = pivotalApiOptions apiToken
    res <- tryRequest (getWith options $ "https://www.pivotaltracker.com/services/v5/stories/" ++ storyId) 

    case res of 
      (Right response) -> do 
        let pivotalProjectId = response ^? responseBody . key "project_id"
        case pivotalProjectId of 
          Just (Number projectId) -> return . Just $ PivotalStory (coefficient projectId) $ T.pack storyId
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




storyPostUrl :: PivotalStory -> String
storyPostUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (projectId story), "/stories/", T.unpack (storyId story),  "/labels"] 

tagStory :: Tag -> PivotalStory -> IO ()
tagStory tag story = do 
    apiToken <- getApiToken
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    let formBody = "name" := tag
    response <- (tryRequest $ postWith requestOptions (storyPostUrl story) formBody)
    case response of 
      Right res -> BL.putStrLn $ res ^. responseBody
      Left (StatusCodeException status headers _) -> do
        putStrLn $ "Error! status code: " ++ (show status)

