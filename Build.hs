module Build() where

import Package
import qualified System

import qualified Data.Map as Map
import Data.Hash

type BuildId = Hash

data BuildData = BuildData { 
                             package :: PackageName,
                             packageDependencies :: [PackageName],
                             buildDependencies :: [BuildId]
                           }

--Build dependencies should not count towards hash as equality should be determined by packacge and package dependencies alone
getId :: BuildData -> BuildId
getId buildData = package' `combine` packageDependencies'
    where package' = hash $ package buildData
          packageDependencies' = hash $  packageDependencies buildData

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail          -- Build failed in progress of building this package.
                 deriving(Eq,Show,Ord)

data Build = Build BuildData BuildResult
            
 --We use a lazy map here so we only need to run a build when we want to query a result
data BuildDatabase = BuildDatabase {
                                     resultMap  :: (Map.Map BuildId BuildResult),-- Map of a build data to the result.
                                     primaryMap :: (Map.Map PackageName BuildData),-- Map of package name to the primary build associated to it
                                     idMap ::      (Map.Map BuildId BuildData)     -- Map of a buildId to it's associated build data
                                   }

--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
fromPackageDatabase :: PackageDatabase -> IO BuildDatabase
fromPackageDatabase = undefined

--Sub contructors used internally in this module to construct the package database from the system

--Create an empty build database
emptyBuildDatabase :: BuildDatabase
emptyBuildDatabase = BuildDatabase Map.empty Map.empty Map.empty

--Add the following build result to the build database
addBuildResult :: BuildData -> BuildResult -> BuildDatabase -> BuildDatabase
addBuildResult  buildData buildResult database = database{ resultMap = resultMap' }
    where resultMap' = Map.insert (getId buildData) buildResult $ resultMap database


--Build the following package on the system and resutrn the build result
-- If the results are already in the build database it will return that.
--First lookup to see if the resut is in the database if so return it.
build :: BuildDatabase -> BuildData -> IO BuildResult
build database buildData = case Map.lookup buildId (resultMap database) of
                                  (Just result) -> return result
                                  --If package is not in database find the build type for all dependencies
                                  Nothing -> do dependentResults <- mapM (build database) dependentBuilds
                                                if all success dependentResults then
                                                    --If all dependencies succeed then build package on the system.
                                                    fmap resultType $ System.build buildPackageList
                                                else
                                                    --If any fail then return a dependence failure
                                                    return DependentFailure
        where buildId = getId buildData
              --Get the build data of the dependent builds
              dependentBuilds = map (getBuildDataFromId database) $ buildDependencies buildData
              --If a result is classed as successful and a translation of the system build into a result type
              success = (== BuildSuccess)
              resultType True = BuildSuccess
              resultType False = BuildFail
              --The package names of all the packages to use in this build
              buildPackageList = package buildData : map package dependentBuilds


--Return the build id associated to the given buildId
getBuildDataFromId :: BuildDatabase -> BuildId -> BuildData
getBuildDataFromId database buildId = (Map.!) ( idMap database ) buildId


--Helper function constructs a BuildData from a package  


--Properties of the BuildDatabase that can be extracted

--Get all the builds in the build database
getAllBuilds :: BuildDatabase -> [Build]
getAllBuilds = undefined

--Get all the builds in the build database that are the primary builds of a package
-- And thus have no restriction on their dependencies
getPrimaryBuilds :: BuildDatabase -> [Build]
getPrimaryBuilds = undefined

--Get the build in the build database that is the primary build associatede to the package in the package 
-- database with the given name.
getPrimaryBuild :: BuildDatabase -> PackageName -> Build
getPrimaryBuild = undefined
