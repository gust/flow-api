{-# LANGUAGE OverloadedStrings #-}
import qualified Web.Scotty as WS
import Data.Monoid
import Control.Monad(liftM)
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








{- main = scotty 3000 $ do -}
  {- post "/" $ do -}
    {- beam <- param "word" -}
    {- html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"] -}

type CommitMessage = String
type StoryId = String
data PivotalStory = PivotalStory { projectId ::  Integer, storyId :: T.Text } deriving Show


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
        let pivotalProjectId =  response ^? responseBody . key "project_id"
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



getStories :: IO [PivotalStory]
getStories = liftM MB.catMaybes $ liftM (storyIdsFromCommits . lines) (readFile "release.txt") >>= pivotalStories


tagStories :: [PivotalStory] -> IO ()
tagStories = mapM_ tagStory 

storyPostUrl :: PivotalStory -> String
storyPostUrl story = concat ["https://www.pivotaltracker.com/services/v5/projects/", show (projectId story), "/stories/", T.unpack (storyId story),  "/labels"] 

tagStory :: PivotalStory -> IO ()
tagStory story = do 
    apiToken <- getApiToken
    let requestOptions = (pivotalApiOptions apiToken) & header "Content-Type" .~ ["application/json"]
    let formBody = BCH.pack "name" := ("super-butts" :: String)
    res <- tryRequest $ postWith requestOptions (storyPostUrl story) formBody
    case res of 
      Right response -> putStrLn "Success"
      Left response -> print res

main :: IO () 
main = getStories >>= tagStories
