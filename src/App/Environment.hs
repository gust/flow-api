module App.Environment(Environment(..)) where
import qualified Data.ByteString.Char8 as BCH

data Environment = Environment {
  pivotalTrackerApiToken :: BCH.ByteString
, slackEndpoint :: String
}
