{-# LANGUAGE OverloadedStrings    #-}
module App.SlackIntegration(notifyNewRelease) where
import Data.Text(Text, append)
import Data.HashMap.Strict(insert, empty, HashMap)
import Data.Aeson(Value(Object, String), toJSON)
import qualified Data.Aeson
import qualified Data.ByteString as BC
import Control.Lens((.~), (&))
import Network.Wreq(postWith, defaults, header, FormParam(..))

newReleaseMessage :: Text -> Text
newReleaseMessage releaseId = "New release, http://zephyr-flow-api.herokuapp.com#/releases/" `append` releaseId

type ReleaseId = Text

opts = defaults & header "Content-Type" .~ ["application/json"]

slackParams :: ReleaseId -> Value
slackParams releaseId = Object payload where
  payload :: HashMap Text Value
  payload = insert "text" (String $ newReleaseMessage releaseId) empty


notifyNewRelease slackEndpoint releaseId = postWith opts slackEndpoint (toJSON $ slackParams releaseId)
