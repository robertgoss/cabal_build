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
  emptyBuildDatabase :: db

  addId :: BuildId -> BuildData -> db -> db
  addResult :: BuildId -> BuildResult -> db -> db
  addPrimary :: PackageName -> BuildId -> db -> db

  getData :: db -> BuildId -> BuildData
  getResult :: db -> BuildId -> BuildResult
  allIds :: db -> [BuildId]



--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
--  First we add all of the builddata for the primary packages then we fold across the build data adding 
--  it for each sub package.
fromPackageDatabase :: (PackageDatabase pkgDb,BuildDatabase buildDb) => pkgDb -> IO buildDb
fromPackageDatabase packageDatabase = buildDatabaseWithPackageData >>= buildAll
    where buildDatabaseWithPackageData = addAllPrimaryBuildData packageDatabase emptyBuildDatabase

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
                                                            return $ addPrimary name buildId newBuildDatabase

    where addBasedOnDeps deps = case deps of --Add based on if there has been resolutin failure
                                    -- The primary build is just a non-primary build data in 
                                    -- the context given by it's dependencies
                                    (Just context) -> addNonPrimaryBuildData context name packageDatabase buildDatabase
                                    --There has been a resolution failure. Add in the stub build
                                    Nothing -> let stubData = BuildData name Nothing Nothing
                                                   stubId = getId stubData
                                               in return (stubId, addId stubId stubData buildDatabase)




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
              --To avoid readding data to the database we see if the id associated to this build is in the dataase already
              if (getResult buildDatabase buildId) /= NotBuilt then 
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
                                                     return (buildId, addId buildId buildData buildDatabaseDeps)

          where depIter (depIds,depDb) depName = do (depId,dbDep) <- addNonPrimaryBuildData context depName packageDatabase depDb
                                                    return(depId:depIds, dbDep)




--Takes gets the given buildId and build database and updates the database 
-- With the build results of this build. If the build result is already know 
-- for this buildId then no changes are made.
-- If any dependent builds are need they are also built and there results
-- Are added to the database as well.
build :: (BuildDatabase buildDb) => BuildId -> buildDb -> IO buildDb
--We first guard against the possibility that a result is already attached to this
-- buildId in which case the database is unchanged
build buildId buildDatabase | (getResult buildDatabase buildId) /= NotBuilt = return buildDatabase
                         --Next we guard against the possibility that the given build had a resolution failure
                            | isNothing (packageDependencies buildData) 
                                                   = return $ addResult buildId ResolutionFailure buildDatabase
                         --Otherwise we build all of the dependencies and get the database with there results.
                         -- We lookup the reuslts in this to see if there are any dependence failures
                         -- If there is then we add as a dependence failure else we build the package on the system
                            | otherwise = do dependentBuildDatabase <- buildList dependenceIds buildDatabase
                                             let depResults = map (getResult dependentBuildDatabase) dependenceIds
                                             if all (==BuildSuccess) depResults then 
                                                 return $ addResult buildId DependentFailure buildDatabase
                                             else 
                                                 --Build package on system 
                                                 do buildStatus <- System.build fullPackageList
                                                    if buildStatus then return $ addResult buildId BuildSuccess buildDatabase
                                                                   else return $ addResult buildId BuildFail buildDatabase

    where --The buildData of this id. It is assumes in database
          buildData = getData buildDatabase buildId
          --The Ids of the depenendences use from just as have guarded aganinst resolution failure
          dependenceIds = fromJust $ buildDependencies buildData
          -- The full package list to build
          fullPackageList = package buildData : fromJust (packageDependencies buildData)


--Helper function build a list of build Ids as in build. But chain together the databases so the final
-- Database has the results of building all the packages. Use monadinc fold
buildList :: (BuildDatabase buildDb) => [BuildId] -> buildDb -> IO buildDb
buildList buildIds buildDatabase = foldM build' buildDatabase buildIds
  where build' a b = build b a -- Build needs an argument swap to work with foldM

--Build all builds in the build database 
buildAll :: (BuildDatabase buildDb) => buildDb -> IO buildDb
buildAll buildDatabase = foldM build' buildDatabase $ zip indices buildIds
  where build' db (index, buildId) = let packageName = package $ getData db buildId
                                     in do putStrLn $ format "{0}/{1} - {2}" [show index, totalBuilds, packageName]
                                           build buildId db -- Build needs an argument swap to work with foldM
        totalBuilds = show $ length buildIds
        indices = [1..] :: [Int]
        buildIds = allIds buildDatabase


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
