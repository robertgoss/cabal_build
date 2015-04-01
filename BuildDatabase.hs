{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module BuildDatabase(BuildDatabaseSqlite,
                     loadSqliteBuildDatabase,
                     BuildDatabasePostgres,
                     loadPostgresBuildDatabase,) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe(fromJust, isJust)
import Debug.Trace(trace)

import Control.Monad.Trans.Resource(runResourceT,ResourceT)
import Control.Monad.Logger(runNoLoggingT,NoLoggingT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Build
import qualified Package
import qualified BuildResult

import qualified Data.Set as Set

import qualified Data.Text as T

--Indexes 

--persistent cannot create objects for speed should have them on:
--  BuildData - buildHash
--  Result - buildId
--  Primary package
--  BuildDependance build
--  PackageDependance build

--Setup a datatype to store build data as persist objects
-- Also store the results map, id map and primary map
-- This may not be the best way but closly follows the in memory method
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BuildData
    package Package.PackageName --Store the full package name
    resolutionFailed Bool -- Stores if a resolution has failed
    buildHash Build.BuildId
    nDepends Int -- Stores a cache of the number of dependencies.
    UniqueId buildHash  -- The buildhash should be unique.
    deriving Show
Result
    buildId Build.BuildId
    buildResult BuildResult.BuildResult
    UniqueResultId buildId
    deriving Show
Primary
    package Package.PackageName
    buildId Build.BuildId 
    UniquePackage package 
    deriving Show
BuildDependance -- Expresses relation of a build being dependant on another.
    build BuildDataId
    dependant Build.BuildId
PackageDependance -- Expresses relation of a build being dependant on a package.
    build BuildDataId
    dependant Package.PackageName
    deriving Show
|] 


--Database class - as we do not export the construtor the database can only be accessed through load and empty
-- And can only be modified though build database after this.
--Sqlite database
data BuildDatabaseSqlite = BuildDatabaseSqlite

--Make sure migrations are run in load
loadSqliteBuildDatabase :: IO BuildDatabaseSqlite
loadSqliteBuildDatabase = do runSqlite "build-sqlite.data" $ runMigration migrateAll
                             return BuildDatabaseSqlite

--Hardcoded sqlite file
sqliteFile = "build-sqlite.data"

instance Build.BuildDatabase BuildDatabaseSqlite where
    emptyBuildDatabase = runSqlite sqliteFile $ do runMigration migrateAll
                                                   return $ BuildDatabaseSqlite 

    addId buildId buildData _ = runSqlite sqliteFile $ do addIdQuery buildId buildData
                                                          return BuildDatabaseSqlite

    addResult buildId buildResult db = runSqlite sqliteFile $ do addResultQuery buildId buildResult
                                                                 return db
    addPrimary package buildId db = runSqlite sqliteFile $ do addPrimaryQuery package buildId
                                                              return db

    getData _ buildId = runSqlite sqliteFile $ getDataQuery buildId

    getResult _ buildId = runSqlite sqliteFile $ getResultQuery buildId

    allIds _ = runSqlite sqliteFile $ allIdsQuery

    idExists _ buildId = runSqlite sqliteFile $ idExistsQuery buildId

--Database class - as we do not export the construtor the database can only be accessed through load and empty
-- And can only be modified though build database after this.
--Postgres database
data BuildDatabasePostgres = BuildDatabasePostgres

--Make sure migrations are run in load
loadPostgresBuildDatabase :: IO BuildDatabasePostgres
loadPostgresBuildDatabase = do runSqlite "build-sqlite.data" $ runMigration migrateAll
                               return BuildDatabasePostgres

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

instance Build.BuildDatabase BuildDatabasePostgres where
    emptyBuildDatabase = runPostgres $ do runMigration migrateAll
                                          return BuildDatabasePostgres

    addId buildId buildData _ = runPostgres $ do addIdQuery buildId buildData
                                                 return BuildDatabasePostgres

    addResult buildId buildResult db = runPostgres $ do addResultQuery buildId buildResult
                                                        return db
    addPrimary package buildId db = runPostgres $ do addPrimaryQuery package buildId
                                                     return db

    getData _ buildId = runPostgres $ getDataQuery buildId

    getResult _ buildId = runPostgres $ getResultQuery buildId

    allIds _ = runPostgres allIdsQuery

    idExists _ buildId = runPostgres $ idExistsQuery buildId

--Required Queries
--Get database ids - no filter only return name
-- Use an conduit and filte the list as go so dont need all in memory
-- Return in order of the number of dependenices from most to least.
allIdsQuery =  selectSource [] [] $$ CL.map onlyName =$ CL.consume
    where onlyName = buildDataBuildHash . entityVal

--Get result find result entity with id and extract result
getResultQuery buildId = do entity' <- getBy $ UniqueResultId buildId 
                            case entity' of
                                (Just entity) -> return . resultBuildResult . entityVal $ entity
                                Nothing -> return BuildResult.NotBuilt

--Get result find entity with id and extract and construct build data 
getDataQuery buildId = do entity <- fmap fromJust $ getBy $ UniqueId buildId 
                          let buildDataId = entityKey entity
                              val = entityVal entity
                              package = buildDataPackage val
                              resFailed = buildDataResolutionFailed val
                          --If resolution failed we can return now else get the dependencies
                          if resFailed then return $ Build.BuildData package Nothing Nothing
                          else do bDependencies' <- selectList [BuildDependanceBuild ==. buildDataId] []
                                  pDependencies' <- selectList [PackageDependanceBuild ==. buildDataId] []
                                  --Get build and ackage dependance entities.
                                  let bDependencies = map (buildDependanceDependant . entityVal) bDependencies'
                                      pDependencies = map (packageDependanceDependant . entityVal) pDependencies'
                                  return $ Build.BuildData package (Just pDependencies) (Just bDependencies)

--Add id by construction sqlite build data and inserting it
--Guard against repeat adds -> The guard now performed by set in database
-- Also insert into result table
addIdQuery buildId buildData = do buildSqliteId <- insertUnique buildDataSqlite
                                  case buildSqliteId of
                                    Nothing -> return () -- This id is already in database
                                    (Just bid) -> do insertUnique $ Result buildId BuildResult.NotBuilt
                                                     case buildDependencies' of
                                                       Nothing -> return () 
                                                       otherwise -> do insertMany_ . map (BuildDependance bid) $ deps -- If there are dependencies insert relations 
                                                                       insertMany_ . map (PackageDependance bid) $ pDeps

    where package' = Build.package buildData
          buildDependencies' = Build.buildDependencies buildData
          packageDependencies' = Build.packageDependencies buildData
          resFailure = buildDependencies' == Nothing -- A resolution has failed if buildDependencies contains nothing
          buildDataSqlite = BuildData package' resFailure buildId nDeps-- The buildData
          --From just dep and pdeps -- only use after resolution shown to exist
          deps = fromJust buildDependencies'
          pDeps = fromJust packageDependencies'
          nDeps = case (buildDependencies') of  --Get the number of dependencies set to 0 is there is a resolution failure.
                     Nothing -> 0
                     (Just ds) -> length ds

--Add result by finding result with build id and updating it's build result
addResultQuery buildId buildResult = do entity <- fmap fromJust $ getBy $ UniqueResultId buildId
                                        update (entityKey entity) [ResultBuildResult =. buildResult]

--Insert a link by constructing an element of the primary table
addPrimaryQuery package buildId = insert $ Primary package buildId

--See just if this id exists in the database
idExistsQuery buildId = fmap isJust . getBy $ UniqueId buildId