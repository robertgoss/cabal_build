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
PackageSqlite
    name Package.PackageName -- Allow pulling name list out of database without reconstructing dependencies
    resolutionFailed Bool -- If the resolution failed
    backjumpReached Bool -- If the backjump limit was reached during resolution
                         -- This may mean with more resources a resolution can be found.
    installed Bool --If this package is installed on the system
    nDepends Int -- Cached result - number of dependences this package has.
    UniqueName name
    deriving Show 
PackageDependence
    package PackageSqliteId
    dependant Package.PackageName
    deriving Show
Latest -- A map of the latest version of a package
    name String -- The name of package without version
    latest PackageSqliteId -- Assosiated latest package
    UniqueLatestName name
|]


--We do not expose this constructor so the only availible constructor creates an empty database
--Keep a set of the current packages
data PackageDatabaseSqlite = PackageDatabaseSqlite 

sqliteFile = "package-sqlite.data"

--Mke sure migrations are run in load
loadSqlitePackageDatabase :: IO PackageDatabaseSqlite
loadSqlitePackageDatabase = runSqlite sqliteFile $ do runMigration migrateAll
                                                      return PackageDatabaseSqlite

--Convert PackageDatabaseSqlite to be a packagedatabase
-- Here we use a constant path to the database for now.
instance Package.PackageDatabase PackageDatabaseSqlite where
    emptyDatabase = runSqlite sqliteFile $ do runMigration migrateAll
                                              return PackageDatabaseSqlite

    --Get a source of package names
    -- Source means they dont all need to be loaded into memory - only id's need to be in memory
    -- This is done in order (least to most of dependenices)
    packageNameSource _ = do ids <- runSqlite sqliteFile $ selectKeysList [] [Asc PackageSqliteNDepends]
                             return $ CL.sourceList ids $= CL.mapM getPackage
          where getPackage pid = runSqlite sqliteFile . fmap (packageSqliteName . fromJust) $ get pid

    insert name deps _ = runSqlite sqliteFile $ do insertQuery name deps
                                                   return PackageDatabaseSqlite
    getDependency _ name = runSqlite sqliteFile $ dependenceQuery name

    latest _ name = runSqlite sqliteFile $ latestQuery name

    makeLatest name _ = runSqlite sqliteFile $ do makeLatestQuery name
                                                  return PackageDatabaseSqlite

    --Get a source of package names
    -- Source means they dont all need to be loaded into memory - only id's need to be in memory
    latestPackageNameSource _ = do ids <- runSqlite sqliteFile $ selectSource [] [] $= CL.map getId $$ CL.consume
                                   return $ CL.sourceList ids $= CL.mapM getPackage
          where getPackage pid = runSqlite sqliteFile . fmap (packageSqliteName . fromJust) $ get pid
                getId = latestLatest . entityVal




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

--Convert PackageDatabaseSqlite to be a packagedatabase
-- Here we use a constant path to the database for now.
instance Package.PackageDatabase PackageDatabasePostgres where
    emptyDatabase = runPostgres $ do runMigration migrateAll
                                     return PackageDatabasePostgres

    --Get a source of package names
    -- Source means they dont all need to be loaded into memory - only id's need to be in memory
    -- This is done in order (least to most of dependenices)
    packageNameSource _ = do ids <- runPostgres $ selectKeysList [] [Asc PackageSqliteNDepends]
                             return $ CL.sourceList ids $= CL.mapM getPackage
          where getPackage pid = runPostgres . fmap (packageSqliteName . fromJust) $ get pid

    insert name deps _ = runPostgres $ do insertQuery name deps
                                          return PackageDatabasePostgres
    getDependency _ name = runPostgres $ dependenceQuery name

    latest _ name = runPostgres $ latestQuery name

    makeLatest name _ = runPostgres $ do makeLatestQuery name
                                         return PackageDatabasePostgres

    --Get a source of package names
    -- Source means they dont all need to be loaded into memory - only id's need to be in memory
    latestPackageNameSource _ = do ids <- runPostgres $ selectSource [] [] $= CL.map getId $$ CL.consume
                                   return $ CL.sourceList ids $= CL.mapM getPackage
          where getPackage pid = runPostgres . fmap (packageSqliteName . fromJust) $ get pid
                getId = latestLatest . entityVal




--Required queries

--Insert based on type of dependence
insertQuery name Package.NotResolved = insert_ $ PackageSqlite name True False False 0 
insertQuery name Package.ResolutionUnknown = insert_ $ PackageSqlite name True True False 0
insertQuery name Package.Installed = insert_ $ PackageSqlite name True False True 0
--Resolution succeded so also add dependence relations.
insertQuery name (Package.Dependencies deps) = do pkgId <- insert $ PackageSqlite name False False False (length deps)
                                                  insertMany_ $ zipWith PackageDependence (repeat pkgId) deps

dependenceQuery name = do package' <- getBy $ UniqueName name
                          case package' of 
                            Nothing -> return Nothing -- Could not find the package returning nothing
                            Just package -> if not (resolutionFailed package) -- Resolution succeded return dependencies 
                                                                              -- Get dependencies with query then wrap
                                                then do deps <- getDependenceQuery $ entityKey package 
                                                        return . Just . Package.Dependencies $ deps
                                                else -- Resolution failedfind out why and return answer
                                                     --Chek through other resound that resolution may have failed
                                                     if installed package then return $ Just Package.Installed
                                                                          else if backjump package then return $ Just Package.ResolutionUnknown
                                                                                                   else return $ Just Package.NotResolved 
    where getDependenceQuery packageId = do dependencies <- selectList [PackageDependencePackage ==. packageId] []
                                            return . Prelude.map (packageDependenceDependant . entityVal) $ dependencies
          resolutionFailed = packageSqliteResolutionFailed . entityVal
          installed = packageSqliteInstalled . entityVal
          backjump = packageSqliteBackjumpReached . entityVal

latestQuery name = do latest' <- getBy $ UniqueLatestName name
                      case latest' of
                          Nothing -> return Nothing
                          (Just latest) -> do package <- get . latestLatest $ entityVal latest
                                              return . Just . packageSqliteName . fromJust $ package

makeLatestQuery packageName = do package' <- getBy $ UniqueName packageName
                                 case package' of
                                      Nothing -> return ()
                                      Just package -> do deleteBy $ UniqueLatestName name -- Remove any old latest record
                                                         insert_ $ Latest name (entityKey package) -- Insert new record with current latest version.
    where name = fst $ Package.splitPackageName packageName
