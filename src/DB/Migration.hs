module DB.Migration(runMigrations) where
import DB.DB(runDbIO)
import DB.Schema(migrateAll)
import Database.Persist.Postgresql(runMigrationUnsafe)

runMigrations connString =  runDbIO connString (runMigrationUnsafe migrateAll)
