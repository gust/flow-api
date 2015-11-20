{-# LANGUAGE GADTs    #-}
module DB.DB(runDbIO, insertIfNew, keyFromParam) where 
import Database.Persist
import qualified Data.ByteString.Char8 as BCH
import Database.Persist.Postgresql
import Control.Monad.Trans(liftIO)
import Control.Monad.Logger(runStdoutLoggingT)
import Control.Monad.Trans(MonadIO,liftIO)
import Control.Monad.Trans.Reader( ReaderT(..))

runDbIO :: BCH.ByteString -> SqlPersistM a -> IO a
runDbIO connStr statement = runStdoutLoggingT $ withPostgresqlConn connStr $ \connection ->
          liftIO $ runSqlPersistM statement connection

insertIfNew :: (MonadIO m, PersistEntityBackend val ~ backend, PersistEntity val, PersistUnique backend) => val -> ReaderT backend m (Entity val)
insertIfNew = flip upsert []


keyFromParam = toSqlKey . fromIntegral . read
