module PivotalTracker where

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

