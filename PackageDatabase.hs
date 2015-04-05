{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module PackageDatabase(loadSqlitePackageDatabase, PackageDatabaseSqlite,
                       loadPostgresPackageDatabase, PackageDatabasePostgres) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe(fromJust)
import Debug.Trace(trace)
import Control.Monad.Trans.Resource(runResourceT,ResourceT)
import Control.Monad.Logger(runNoLoggingT,NoLoggingT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Package 

type VersionString = String

--Setup a datatype to store packages as persist objects


--Indexes 

--persistent cannot create objects for speed should have them on PackageDependence Package
--                                                                 PackageSqite Name

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Package
    -- Allow pulling name list out of database without reconstructing dependencies
    packageType Package.PackageType
    packageVersion [Int] -- We have to store the version data as a string 
                         -- Auto converted by persistent
    fetched Bool -- If this package has been fetched on the system. Default that it hasn't.
    UniqueName packageType packageVersion -- The package name is a combination of these two and is unique as a pair
    deriving Show 
PackagePureDependence
    package PackageId
    dependant Package.PackageType -- The the package types that this package will rely on.
    deriving Show
PackageLatest -- A map of the latest version of a package
    name Package.PackageType -- The name of package without version
    latest PackageId -- Assosiated latest package
    UniqueLatestType name
|]

--Helper function to get a PakageName from a Package
getName :: Package -> Package.PackageName
getName package = Package.PackageName (packagePackageType package) (packagePackageVersion package)


--We do not expose this constructor so the only availible constructor creates an empty database
--Keep a set of the current packages
data PackageDatabaseSqlite = PackageDatabaseSqlite 

sqliteFile = "package-sqlite.data"

--Mke sure migrations are run in load
loadSqlitePackageDatabase :: IO PackageDatabaseSqlite
loadSqlitePackageDatabase = runSqlite sqliteFile $ do runMigration migrateAll
                                                      return PackageDatabaseSqlite


--Helper function
getSqlitePackageName key = runSqlite sqliteFile $ do package <- get key
                                                     return . getName . fromJust $ package

--Convert PackageDatabaseSqlite to be a packagedatabase
-- Here we use a constant path to the database for now.
instance Package.PackageDatabase PackageDatabaseSqlite where
    --Load the database by deleting all after a migraion
    emptyDatabase = runSqlite sqliteFile $ do runMigration migrateAll
                                              deleteAllQuery 
                                              return PackageDatabaseSqlite
    addPackage _ package = runSqlite sqliteFile $ addPackageQuery package
    getPackage _ packageName = runSqlite sqliteFile $ getPackageQuery packageName
    fetched _ packageName = runSqlite sqliteFile $ fetchedQuery packageName

    --Get  full list only of the Ids to save space
    -- Cant use the source from persistent as need to mapM in IO
    packageNameSource _ = do keysList <- runSqlite sqliteFile $ selectKeysList [] []
                             return $ CL.sourceList keysList $= CL.mapM getSqlitePackageName 

    --Fetched and unfetched versions of above
    fetchedSource _ = do keysList <- runSqlite sqliteFile $ selectKeysList [PackageFetched ==. True] []
                         return $ CL.sourceList keysList $= CL.mapM getSqlitePackageName 
    unFetchedSource _ = do keysList <- runSqlite sqliteFile $ selectKeysList [PackageFetched ==. False] []
                           return $ CL.sourceList keysList $= CL.mapM getSqlitePackageName 


--We do not expose this constructor so the only availible constructor creates an empty database
--Keep a set of the current packages
--Postgressql
data PackageDatabasePostgres = PackageDatabasePostgres 


--Hardcoded connection string
connectionString = "host=localhost user=cabal_build password=cabal dbname=cabal_build"

--Helper function to run a postgres connection throws away loging
-- Does nt preserve connection between queries
runPostgres :: (MonadBaseControl IO m, MonadIO m)
          => SqlPersistT (NoLoggingT (ResourceT m)) a -- ^ database action
          -> m a
runPostgres = runResourceT
            . runNoLoggingT
            . withPostgresqlConn connectionString
            . runSqlConn

--Mke sure migrations are run in load
loadPostgresPackageDatabase :: IO PackageDatabasePostgres
loadPostgresPackageDatabase = runPostgres $ do runMigration migrateAll
                                               return PackageDatabasePostgres

--Helper function
getPostgresPackageName key = runPostgres $ do package <- get key
                                              return . getName . fromJust $ package

--Convert PackageDatabasePostgres to be a packagedatabase
-- Here we use a constant connection to the database for now.
instance Package.PackageDatabase PackageDatabasePostgres where
    --Load the database by deleting all after a migraion
    emptyDatabase = runPostgres $ do runMigration migrateAll
                                     deleteAllQuery 
                                     return PackageDatabasePostgres
    addPackage _ package = runPostgres $ addPackageQuery package
    getPackage _ packageName = runPostgres $ getPackageQuery packageName
    fetched _ packageName = runPostgres $ fetchedQuery packageName

    --Get  full list only of the Ids to save space
    -- Cant use the source from persistent as need to mapM in IO
    packageNameSource _ = do keysList <- runPostgres $ selectKeysList [] []
                             return $ CL.sourceList keysList $= CL.mapM getPostgresPackageName 

    --Fetched and unfetched versions of above
    fetchedSource _ = do keysList <- runPostgres $ selectKeysList [PackageFetched ==. True] []
                         return $ CL.sourceList keysList $= CL.mapM getPostgresPackageName 
    unFetchedSource _ = do keysList <- runPostgres $ selectKeysList [PackageFetched ==. False] []
                           return $ CL.sourceList keysList $= CL.mapM getPostgresPackageName 



--Required queries

  --Drop all data so we have empty database
-- Use 2 optins so type checker can work out which tables we need
deleteAllQuery = do deleteWhere [PackageFetched ==. True] 
                    deleteWhere [PackageFetched !=. True] 
                    deleteWhere [PackagePureDependenceDependant ==. ""] 
                    deleteWhere [PackagePureDependenceDependant !=. ""]

fetchedQuery packageName = do package' <- getBy $ UniqueName packageType packageVersion
                              case package' of
                                 Nothing -> return ()
                                 (Just package) -> update (entityKey package) [PackageFetched =. True]

  where (Package.PackageName packageType packageVersion) = packageName


addPackageQuery package = do packageKey' <- insertUnique $ Package packageType packageVersion True
                             case packageKey' of
                                 Nothing -> return ()
                                 Just packageKey -> insertMany_ $ map (PackagePureDependence packageKey) pureDependencies
  where (Package.PackageName packageType packageVersion) = Package.name package
        pureDependencies = Package.pureDependencies package
        packageRow = Package packageType packageVersion True

getPackageQuery packageName = do package' <- getBy $ UniqueName packageType packageVersion
                                 case package' of
                                    Nothing -> return Nothing
                                    (Just package) -> do let packageKey = entityKey package
                                                         deps <- selectList [PackagePureDependencePackage ==. packageKey] []
                                                         return . Just $ Package.Package packageName $ map getDep deps
  where (Package.PackageName packageType packageVersion) = packageName
        getDep = packagePureDependenceDependant . entityVal
