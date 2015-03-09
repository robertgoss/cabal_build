module Build() where

import Package
import qualified System

import qualified Data.Map as Map
import Data.Hash
import Data.Maybe(fromJust,isNothing)

type BuildId = Hash

data BuildData = BuildData { 
                             package :: PackageName,
                             packageDependencies :: Maybe [PackageName], --Maybe in dependencies indicates a resolution failure
                             buildDependencies :: Maybe [BuildId]
                           }

--All the other packages which will be installed with a dependency.
type Context = [PackageName]

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
                                     primaryMap :: (Map.Map PackageName BuildId),-- Map of package name to the primary build associated to it
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

--Add the buildData of the following primary build and it's dependencies to the database
--  We also add buildId of this build to the primaryMap
addPrimaryBuildData :: PackageName -> PackageDatabase -> BuildDatabase -> BuildDatabase
addPrimaryBuildData name packageDatabase buildDatabase = newBuildDatabase {primaryMap = primaryMap'}
    where deps = dependencies $ getPackage packageDatabase name
          (buildId, newBuildDatabase) = case deps of
                                            -- The primary build is just a non-primary build data in 
                                            -- the context given by it's dependencies
                                             (Just context) -> addNonPrimaryBuildData context name packageDatabase buildDatabase
                                            --There has been a resolution failure. Add in the stub build
                                             Nothing -> let stubData = BuildData name Nothing Nothing
                                                            stubId = getId stubData
                                                            idMap' = Map.insert stubId stubData $ idMap buildDatabase
                                                        in (stubId, buildDatabase {idMap = idMap'})
          --Add buildId to primarymap so we can find this build
          primaryMap' = Map.insert name buildId $ primaryMap newBuildDatabase



--Add the buildData of the build of the given package in context and it's dependencies to the database
-- We also return the buildId of this build
addNonPrimaryBuildData :: Context -> PackageName -> PackageDatabase -> BuildDatabase -> (BuildId,BuildDatabase)
addNonPrimaryBuildData = undefined

--Add the following build result to the build database
addBuildResult :: BuildData -> BuildResult -> BuildDatabase -> BuildDatabase
addBuildResult  buildData buildResult database = database{ resultMap = resultMap' }
    where resultMap' = Map.insert (getId buildData) buildResult $ resultMap database


--Build the following package on the system and resutrn the build result
-- If the results are already in the build database it will return that.
--First lookup to see if the resut is in the database if so return it.
build :: BuildDatabase -> BuildData -> IO BuildResult
build database buildData
    -- First guard against resolution failures where the dependents are not known
    -- Give this as reason for buildFailure
    | isNothing (buildDependencies buildData) = return ResolutionFailure
    --In this case can assume that buildDependencies are not nothing
    | otherwise = case Map.lookup buildId (resultMap database) of
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
              -- We can use fromJust here as we guard against 
              dependentBuilds = map (getBuildDataFromId database) . fromJust $ buildDependencies buildData
              --If a result is classed as successful and a translation of the system build into a result type
              success = (== BuildSuccess)
              resultType True = BuildSuccess
              resultType False = BuildFail
              --The package names of all the packages to use in this build
              buildPackageList = package buildData : map package dependentBuilds


--Return the build id associated to the given buildId
getBuildDataFromId :: BuildDatabase -> BuildId -> BuildData
getBuildDataFromId database buildId = (Map.!) ( idMap database ) buildId


--Helper functions for constructing a BuildData  
--Filter out only those elements of the context which are dependencies of this package
getDependenciesFromContext :: PackageDatabase -> Context -> PackageName -> [PackageName]
getDependenciesFromContext database context packageName = filter isDependant context
          --A context package is dependent if it is a different version of a known dependency of this package
    where isDependant cPackage = any (differentVersions cPackage) depends
          --If we have a context then dependence resolution cant have failed.
          depends = fromJust . dependencies $ getPackage database packageName



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
