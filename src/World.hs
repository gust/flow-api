module World where
  import Control.Monad.Trans(MonadIO)
  import Control.Exception as E
  import qualified Network.Wreq as NW
  import qualified Data.ByteString.Lazy as BL
  import Network.Wreq(FormParam( (:=) ), defaults, responseBody, header, Response)
  import Network.HTTP.Conduit(HttpException)
  import qualified Network.Wreq.Types as NWT

  class (Monad m, MonadIO m) => World m where
    getWith :: Monad m => NW.Options -> String -> m (Response BL.ByteString)
    postWith :: (Monad m, NWT.Postable b) => NW.Options -> String -> b -> m (Response BL.ByteString)
    deleteWith :: Monad m => NW.Options -> String -> m (Response BL.ByteString)
    tryRequest :: Monad m => m a ->  m (Either HttpException a)

  instance World IO where
    getWith = NW.getWith
    postWith = NW.postWith
    deleteWith = NW.deleteWith
    tryRequest = E.try



