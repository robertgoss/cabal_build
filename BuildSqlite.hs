{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module BuildSqlite(BuildDatabaseSqlite) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Conduit
import Data.Conduit.List
import Data.Maybe(fromJust, isJust)

import Debug.Trace(trace)

import qualified Build
import qualified Package
import qualified BuildResult

import qualified Data.Set as Set

--Setup a datatype to store build data as persist objects
-- Also store the results map, id map and primary map
-- This may not be the best way but closly follows the in memory method
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BuildData
    package Package.PackageName --Store the full package name
    buildDependencies [Build.BuildId] Maybe --PackageDependencies but mkPersist need alternative def.
    packageDependencies [Package.PackageName] Maybe --PackageDependencies but mkPersist need alternative def.
    buildHash Build.BuildId
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
|] 

--Database class - as we do not export the construtor the database can only be accessed through load and empty
-- And can only be modified though build database after this.
data BuildDatabaseSqlite = BuildDatabaseSqlite

--Make sure migrations are run in load
--loadBuildDatabase :: IO BuildDatabaseSqlite
--loadBuildDatabase = do runSqlite "build-sqlite.data" $ runMigration migrateAll
--                       return BuildDatabaseSqlite


instance Build.BuildDatabase BuildDatabaseSqlite where
    emptyBuildDatabase = runSqlite "build-sqlite.data" $ do runMigration migrateAll
                                                            return $ BuildDatabaseSqlite 

    addId buildId buildData _ = runSqlite "build-sqlite.data" $ do addIdQuery buildId buildData
                                                                   return BuildDatabaseSqlite

    addResult buildId buildResult db = runSqlite "build-sqlite.data" $ do addResultQuery buildId buildResult
                                                                          return db
    addPrimary package buildId db = runSqlite "build-sqlite.data" $ do addPrimaryQuery package buildId
                                                                       return db

    getData _ buildId = runSqlite "build-sqlite.data" $ getDataQuery buildId

    getResult _ buildId = runSqlite "build-sqlite.data" $ getResultQuery buildId

    allIds _ = runSqlite "build-sqlite.data" $ allIdsQuery

    idExists _ buildId = runSqlite "build-sqlite.data" $ idExistsQuery buildId

--Required Queries
--Get database ids - no filter only return name
-- Use an conduit and filte the list as go so dont need all in memory
allIdsQuery =  selectSource [] [] $$ Data.Conduit.List.map onlyName =$ consume
    where onlyName = buildDataBuildHash . entityVal

--Get result find result entity with id and extract result
getResultQuery buildId = do entity' <- getBy $ UniqueResultId buildId 
                            case entity' of
                                (Just entity) -> return . resultBuildResult . entityVal $ entity
                                Nothing -> return BuildResult.NotBuilt

--Get result find entity with id and extract and construct build data 
getDataQuery buildId = do entity <- fmap fromJust $ getBy $ UniqueId buildId 
                          let val = entityVal entity
                              package = buildDataPackage val
                              buildDependencies = buildDataBuildDependencies val
                              packageDependencies = buildDataPackageDependencies val
                          return $ Build.BuildData package packageDependencies buildDependencies

--Add id by construction sqlite build data and inserting it
--Guard against repeat adds -> The guard now performed by set in database
-- Also insert into result table
addIdQuery buildId buildData = do insertUnique buildDataSqlite
                                  insertUnique $ Result buildId BuildResult.NotBuilt
    where package' = Build.package buildData
          packageDependencies' = Build.packageDependencies buildData
          buildDependencies' = Build.buildDependencies buildData
          buildDataSqlite = BuildData package' buildDependencies' packageDependencies' buildId

--Add result by finding result with build id and updating it's build result
addResultQuery buildId buildResult = do entity <- fmap fromJust $ getBy $ UniqueResultId buildId
                                        update (entityKey entity) [ResultBuildResult =. buildResult]

--Insert a link by constructing an element of the primary table
addPrimaryQuery package buildId = insert $ Primary package buildId

--See just if this id exists in the database
idExistsQuery buildId = fmap isJust . getBy $ UniqueId buildId