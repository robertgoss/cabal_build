module Build(emptyBuildDatabase,
             addAllPrimaryBuildData) where

import Package
import qualified System

import Data.Hash
import Data.Maybe(fromJust,isNothing)
import Control.Monad(foldM)
import Text.Format(format)
import Data.List(sort,intersperse)
import Data.Word(Word64)

import Debug.Trace(trace)

type BuildId = Hash


data BuildData = BuildData { 
                             package :: PackageName,
                             packageDependencies :: Maybe [PackageName], --Maybe in dependencies indicates a resolution failure
                             buildDependencies :: Maybe [BuildId]
                           }

instance Show BuildData where
    show buildData | isNothing $ packageDependencies buildData = package buildData ++ " *resolution failed*"
                   | otherwise = concat $ package buildData : " $ " : (intersperse " " . fromJust . packageDependencies $ buildData)


--All the other packages which will be installed with a dependency.
type Context = [PackageName]

--Build dependencies should not count towards hash as equality should be determined by packacge and package dependencies alone
getId :: BuildData -> BuildId
getId buildData = package' `combine` packageDependencies'
    where package' = hash $ package buildData
          --Sort this list as ordering should not matter for equality.
          packageDependencies' = hash . fmap sort $  packageDependencies buildData

getIdFromPackages :: PackageName -> [PackageName] -> BuildId
getIdFromPackages name deps = package' `combine` packageDependencies'
    where package' = hash name
          --Sort this list as ordering should not matter for equality.
          packageDependencies' = hash . fmap sort $  deps

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail          -- Build failed in progress of building this package.
                 | NotBuilt           -- The package has not yet been built
                 deriving(Eq,Show,Ord,Enum)

data Build = Build BuildData BuildResult
            
--An abstraction of the features of a package database held in memory
class BuildDatabase db where
  emptyBuildDatabase :: IO db

  addId :: BuildId -> BuildData -> db -> IO db
  addResult :: BuildId -> BuildResult -> db -> IO db
  addPrimary :: PackageName -> BuildId -> db -> IO db

  getData :: db -> BuildId -> IO BuildData
  getResult :: db -> BuildId -> IO BuildResult
  allIds :: db -> IO [BuildId]



--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
--  First we add all of the builddata for the primary packages then we fold across the build data adding 
--  it for each sub package.
fromPackageDatabase :: (PackageDatabase pkgDb, BuildDatabase buildDb) => pkgDb -> IO buildDb
fromPackageDatabase packageDatabase = do empty <- emptyBuildDatabase
                                         buildDatabaseWithPackageData <- addAllPrimaryBuildData packageDatabase empty
                                         buildAll buildDatabaseWithPackageData

--Sub contructors used internally in this module to construct the package database from the system

--Add the buildData for all the packages in the packageDatabase as primary builds to the 
-- build database use a fold to update the database over the packagelist
addAllPrimaryBuildData :: (PackageDatabase db, BuildDatabase buildDb) => db -> buildDb -> IO buildDb
addAllPrimaryBuildData packageDatabase buildDatabase = do packages <- packageList packageDatabase
                                                          let packageNames = map packageName packages
                                                          foldM addPrimaryIter buildDatabase packageNames
    where addPrimaryIter currBuildDatabase currPackage = trace currPackage $ addPrimaryBuildData currPackage packageDatabase currBuildDatabase


--Add the buildData of the following primary build and it's dependencies to the database
--  We also add buildId of this build to the primaryMap
addPrimaryBuildData :: (PackageDatabase db,BuildDatabase buildDb) 
                       => PackageName -> db -> buildDb -> IO buildDb
addPrimaryBuildData name packageDatabase buildDatabase = do package <- getPackage packageDatabase name
                                                            (buildId, newBuildDatabase) <- addBasedOnDeps $ dependencies package
                                                            addPrimary name buildId newBuildDatabase

    where addBasedOnDeps deps = case deps of --Add based on if there has been resolutin failure
                                    -- The primary build is just a non-primary build data in 
                                    -- the context given by it's dependencies
                                    (Just context) -> addNonPrimaryBuildData context name packageDatabase buildDatabase
                                    --There has been a resolution failure. Add in the stub build
                                    Nothing -> let stubData = BuildData name Nothing Nothing
                                                   stubId = getId stubData
                                               in do databaseWithId <- addId stubId stubData buildDatabase
                                                     return (stubId, databaseWithId)




--Add the buildData of the build of the given package in context and it's dependencies to the database
-- We also return the buildId of this build
-- In this it is assumed that as it is being built in a package context that it has not failed to resolve.
--We first get the build data for the dependencies
addNonPrimaryBuildData :: (PackageDatabase db,BuildDatabase buildDb)
                           => Context -> PackageName -> db -> buildDb -> IO (BuildId,buildDb)
addNonPrimaryBuildData context name packageDatabase buildDatabase
         = do depends <- getDependenciesFromContext packageDatabase context name
              let --Can get id without having to check all sub-packages which is faster
                buildId = getIdFromPackages name depends
              buildResult <- getResult buildDatabase buildId
              --To avoid readding data to the database we see if the id associated to this build is in the dataase already
              if buildResult /= NotBuilt then 
                return (buildId, buildDatabase)
              else
                addNonPrimaryBuildData' depends buildId
          
  where addNonPrimaryBuildData' depends buildId = do --Use a fold to keep updating the build database
                                                     (dependIds,buildDatabaseDeps) <- foldM depIter ([],buildDatabase) depends
                                                     let --Use the dependence data to construct the buildData for this package
                                                         buildData = BuildData { package = name,
                                                                                 buildDependencies = Just dependIds,
                                                                                 packageDependencies = Just depends
                                                                               }
                                                     databaseWithId <- addId buildId buildData buildDatabaseDeps
                                                     return (buildId, databaseWithId)

          where depIter (depIds,depDb) depName = do (depId,dbDep) <- addNonPrimaryBuildData context depName packageDatabase depDb
                                                    return(depId:depIds, dbDep)




--Takes gets the given buildId and build database and updates the database 
-- With the build results of this build. If the build result is already know 
-- for this buildId then no changes are made.
-- If any dependent builds are need they are also built and there results
-- Are added to the database as well.
build :: (BuildDatabase buildDb) => BuildId -> buildDb -> IO buildDb
--We get the result and buildData as these must be done in IO then pass to build'
build buildId database = do result <- getResult database buildId
                            bData <- getData database buildId
                            build' buildId database result bData
--We first guard against the possibility that a result is already attached to this
-- buildId in which case the database is unchanged
build' buildId database result bData | result /= NotBuilt = return database
                                     --Next we guard against the possibility that the given build had a resolution failure
                                     | isNothing (buildDependencies bData) = addResult buildId ResolutionFailure database
                                     --Otherwise we build all of the dependencies and get the database with there results.
                                     -- We lookup the reuslts in this to see if there are any dependence failures
                                     -- If there is then we add as a dependence failure else we build the package on the system
                                     | otherwise = do dependentBuildDatabase <- buildList dependenceIds database
                                                      depResults <- mapM (getResult dependentBuildDatabase) dependenceIds
                                                      if all (==BuildSuccess) depResults then 
                                                         addResult buildId DependentFailure database
                                                      else 
                                                          buildOnSystem

    where --The Ids of the depenendences use from just as have guarded aganinst resolution failure
          dependenceIds = fromJust $ buildDependencies bData
          -- The full package list to build
          fullPackageList = package bData : fromJust (packageDependencies bData)
          --Build package on the system and return the result
          buildOnSystem = do buildStatus <- System.build fullPackageList
                             if buildStatus then addResult buildId BuildSuccess database
                                            else addResult buildId BuildFail database


--Helper function build a list of build Ids as in build. But chain together the databases so the final
-- Database has the results of building all the packages. Use monadinc fold
buildList :: (BuildDatabase buildDb) => [BuildId] -> buildDb -> IO buildDb
buildList buildIds buildDatabase = foldM build' buildDatabase buildIds
  where build' a b = build b a -- Build needs an argument swap to work with foldM

--Build all builds in the build database 
buildAll :: (BuildDatabase buildDb) => buildDb -> IO buildDb
buildAll buildDatabase = do buildIds <- allIds buildDatabase
                            let totalBuilds = show $ length buildIds
                            foldM (build' totalBuilds) buildDatabase $ zip indices buildIds
  where build' totalBuilds db (index, buildId) = do buildData <- getData db buildId
                                                    putStrLn $ format "{0}/{1} - {2}" [show index, totalBuilds, package buildData]
                                                    build buildId db -- Build needs an argument swap to work with foldM
        indices = [1..] :: [Int]


--Helper functions for constructing a BuildData  
--Filter out only those elements of the context which are dependencies of this package
getDependenciesFromContext :: (PackageDatabase db) => db -> Context -> PackageName -> IO [PackageName]
getDependenciesFromContext database context packageName = do package <- getPackage database packageName
                                                             --If we have a context then dependence resolution cant have failed.
                                                             let depends = fromJust . dependencies $ package
                                                             return $ filter (isDependant depends) context
          --A context package is dependent if it is a different version of a known dependency of this package
    where isDependant depends cPackage = any (differentVersions cPackage) depends




--Properties of the BuildDatabase that can be extracted

--Get all the builds in the build database
getAllBuilds :: (BuildDatabase buildDb) => buildDb -> [Build]
getAllBuilds = undefined

--Get all the builds in the build database that are the primary builds of a package
-- And thus have no restriction on their dependencies
getPrimaryBuilds :: (BuildDatabase buildDb) => buildDb -> [Build]
getPrimaryBuilds = undefined

--Get the build in the build database that is the primary build associatede to the package in the package 
-- database with the given name.
getPrimaryBuild :: (BuildDatabase buildDb) => buildDb -> PackageName -> Build
getPrimaryBuild = undefined
