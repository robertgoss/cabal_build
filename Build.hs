module Build() where

import Package
import qualified System

import qualified Data.Map as Map

data BuildData = BuildData { 
                             package :: PackageName,
                             dependencies :: [BuildData]
                           } deriving(Eq,Ord)

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail          -- Build failed in progress of building this package.
                 deriving(Eq,Show,Ord)

data Build = Build BuildData BuildResult
            
 --We use a lazy map here so we only need to run a build when we want to query a result
data BuildDatabase = BuildDatabase (Map.Map BuildData BuildResult) -- Map of a build data to the result.
                                   (Map.Map PackageName BuildData) -- Map of package name to the primary build associated to it

--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
fromPackageDatabase :: PackageDatabase -> IO BuildDatabase
fromPackageDatabase = undefined

--Sub contructors used internally in this module to construct the package database from the system

--Create an empty build database
emptyBuildDatabase :: BuildDatabase
emptyBuildDatabase = BuildDatabase Map.empty Map.empty

--Add the following build to the build database
addBuild :: Build -> BuildDatabase -> BuildDatabase
addBuild (Build buildData buildResult) (BuildDatabase resultMap primaryMap) = BuildDatabase resultMap' primaryMap
    where resultMap' = Map.insert buildData buildResult resultMap

--Build the following package on the system
-- If the results are already in the build database it will return that.
--First lookup to see if the resut is in the database if so return it.
build :: BuildDatabase -> BuildData -> IO BuildResult
build db@(BuildDatabase resultMap _) buildData = case Map.lookup buildData resultMap of
                                                    (Just result) -> return result
                                                      --If package is not in database find the build type for all dependencies
                                                    Nothing -> do dependentResults <- mapM (build db) dependentBuilds
                                                                  if all success dependentResults then
                                                                    --If all dependencies succeed then build package on the system.
                                                                    fmap resultType $ System.build buildPackageList
                                                                  else
                                                                    --If any fail then return a dependence failure
                                                                    return DependentFailure
        where dependentBuilds = dependencies buildData :: [BuildData]
              --If a result is classed as successful and a translation of the system build into a result type
              success = (== BuildSuccess)
              resultType True = BuildSuccess
              resultType False = BuildFail
              --The package names of all the packages to use in this build
              buildPackageList = package buildData : map package dependentBuilds

--Get the primary build data associated to the pacakge with the given name in the package database.
getBuildData :: PackageName -> PackageDatabase -> BuildData
getBuildData = undefined


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
