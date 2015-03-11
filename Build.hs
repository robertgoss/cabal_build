module Build(emptyBuildDatabase, 
             saveBuildDatabase, loadBuildDatabase,
             addAllPrimaryBuildData) where

import Package
import qualified System

import qualified Data.Map as Map
import Data.Hash
import Data.Maybe(fromJust,isNothing)
import Control.Monad(foldM)
import Text.Format(format)
import Data.List(sort,intersperse)
import Data.ByteString as BS(writeFile, readFile)

import Data.Serialize(encode,decode)
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
            
 --We use a lazy map here so we only need to run a build when we want to query a result
data BuildDatabase = BuildDatabase {
                                     resultMap  :: (Map.Map BuildId BuildResult),-- Map of a build data to the result.
                                     primaryMap :: (Map.Map PackageName BuildId),-- Map of package name to the primary build associated to it
                                     idMap ::      (Map.Map BuildId BuildData)     -- Map of a buildId to it's associated build data
                                   }

--Basic IO to load and store the build databases as files. Uses cereal - not sure of 
-- Stabiliy on data change, used to store results of long build proccess

--Save the build database
-- Encode the build database as a tuple of maps as cereal can serials tuples and maps
-- Also we save the build results map with the results as integers as cereal can decode these and buildResults is an Enum
-- Also encode builddata as tuple.
-- Finally as Hash is not serializable save the associated word64
saveBuildDatabase :: BuildDatabase -> IO ()
saveBuildDatabase buildDatabase = BS.writeFile "build.data" $ encode buildTuple
    where buildTuple = (resultMapInt, primaryMapWord, idMapTuple)
          -- Map the various field of the maps to the serializable equivilents.
          resultMapInt = Map.mapKeys asWord64 . Map.map fromEnum $ resultMap buildDatabase
          toDataTuple (BuildData a b c) = (a,b,fmap (map asWord64) c)  -- Convert hash list
          primaryMapWord = Map.map asWord64 $ primaryMap buildDatabase
          idMapTuple = Map.mapKeys asWord64 . Map.map toDataTuple $ idMap buildDatabase

--Load database
-- The database is encoded as a tuple of the maps in build see save.
-- The resultMap has buildresuts encoded as ints
--This throws an error if it cannot decode the database.
loadBuildDatabase :: IO BuildDatabase
loadBuildDatabase = do bytestring <- BS.readFile "build.data"
                       case (decode bytestring) of
                         (Right buildTuple) -> return (fromBuildTuple buildTuple)
  where fromBuildTuple :: (Map.Map Word64 Int, Map.Map String Word64, Map.Map Word64 (String,Maybe [String],Maybe [Word64])) -> BuildDatabase
        fromBuildTuple (resultMapInt, primaryMapWord, idMapTuple) = BuildDatabase resultMap primaryMap idMap 
           where resultMap = Map.mapKeys hashReverse . Map.map toEnum $ resultMapInt
                 idMap = Map.mapKeys hashReverse . Map.map fromDataTuple $ idMapTuple
                 primaryMap = Map.map hashReverse primaryMapWord
                 -- This function reverses the asWord64 used in save by for a given word64 looking up the buildData and taking its hash
                 hashReverse word = getId . fromDataTuple .fromJust $ Map.lookup word idMapTuple 
                 fromDataTuple (a,b,c) = BuildData a b (fmap (map hashReverse) c) -- Convert tuple back to build data. Unconvert asWord

--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
--  First we add all of the builddata for the primary packages then we fold across the build data adding 
--  it for each sub package.
fromPackageDatabase :: (PackageDatabase db) => db -> IO BuildDatabase
fromPackageDatabase packageDatabase = buildAll buildDatabaseWithPackageData
    where buildDatabaseWithPackageData = addAllPrimaryBuildData packageDatabase emptyBuildDatabase

--Sub contructors used internally in this module to construct the package database from the system

--Create an empty build database
emptyBuildDatabase :: BuildDatabase
emptyBuildDatabase = BuildDatabase Map.empty Map.empty Map.empty


--Add the buildData for all the packages in the packageDatabase as primary builds to the 
-- build database use a fold to update the database over the packagelist
addAllPrimaryBuildData :: (PackageDatabase db) => db -> BuildDatabase -> BuildDatabase
addAllPrimaryBuildData packageDatabase buildDatabase = foldl addPrimaryIter buildDatabase packages
    where packages = map packageName $ packageList packageDatabase
          addPrimaryIter currBuildDatabase currPackage = trace currPackage $ addPrimaryBuildData currPackage packageDatabase currBuildDatabase


--Add the buildData of the following primary build and it's dependencies to the database
--  We also add buildId of this build to the primaryMap
addPrimaryBuildData :: (PackageDatabase db) => PackageName -> db -> BuildDatabase -> BuildDatabase
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
-- In this it is assumed that as it is being built in a package context that it has not failed to resolve.
--We first get the build data for the dependencies
addNonPrimaryBuildData :: (PackageDatabase db) => Context -> PackageName -> db -> BuildDatabase -> (BuildId,BuildDatabase)
addNonPrimaryBuildData context name packageDatabase buildDatabase 
         --To avoid readding data to the database we see if the id associated to this build is in the dataase already
         | Map.member buildId $ idMap buildDatabase = (buildId, buildDatabase)
         | otherwise = (buildId,finalDatabase)
    where depends = getDependenciesFromContext packageDatabase context name
          --Can get id without having to check all sub-packages which is faster
          buildId = getIdFromPackages name depends
          --Use a fold to keep updating the build database
          --We return the database with all the dependednt id's added and a list of the dependent ids
          (dependIds,buildDatabaseDeps) = foldl depIter ([],buildDatabase) depends
          depIter (depIds,depDb) depName = (depId:depIds, dbDep)
            where (depId,dbDep) = addNonPrimaryBuildData context depName packageDatabase depDb
          --Use the dependence data to construct the buildData for this package
          buildData = BuildData {
             package = name,
             buildDependencies = Just dependIds,
             packageDependencies = Just depends
          }
          --Add the buildData to the database
          idMap' = Map.insert buildId buildData $ idMap buildDatabaseDeps
          finalDatabase = buildDatabaseDeps { idMap = idMap' }


--Takes gets the given buildId and build database and updates the database 
-- With the build results of this build. If the build result is already know 
-- for this buildId then no changes are made.
-- If any dependent builds are need they are also built and there results
-- Are added to the database as well.
build :: BuildId -> BuildDatabase -> IO BuildDatabase
--We first guard against the possibility that a result is already attached to this
-- buildId in which case the database is unchanged
build buildId buildDatabase | Map.member buildId (resultMap buildDatabase) = return buildDatabase
                         --Next we guard against the possibility that the given build had a resolution failure
                            | isNothing (packageDependencies buildData) 
                                                   = return $ addBuildResult buildDatabase buildId ResolutionFailure 
                         --Otherwise we build all of the dependencies and get the database with there results.
                         -- We lookup the reuslts in this to see if there are any dependence failures
                         -- If there is then we add as a dependence failure else we build the package on the system
                            | otherwise = do dependentBuildDatabase <- buildList dependenceIds buildDatabase
                                             let depResults = map (getBuildResultFromId dependentBuildDatabase) dependenceIds
                                             if all (==BuildSuccess) depResults then 
                                                 return $ addBuildResult buildDatabase buildId DependentFailure
                                             else 
                                                 --Build package on system 
                                                 do buildStatus <- System.build fullPackageList
                                                    if buildStatus then return $ addBuildResult buildDatabase buildId BuildSuccess
                                                                   else return $ addBuildResult buildDatabase buildId BuildFail

    where --The buildData of this id. It is assumes in database
          buildData = fromJust . Map.lookup buildId $ idMap buildDatabase
          --The Ids of the depenendences use from just as have guarded aganinst resolution failure
          dependenceIds = fromJust $ buildDependencies buildData
          -- The full package list to build
          fullPackageList = package buildData : fromJust (packageDependencies buildData)


--Helper function build a list of build Ids as in build. But chain together the databases so the final
-- Database has the results of building all the packages. Use monadinc fold
buildList :: [BuildId] -> BuildDatabase -> IO BuildDatabase
buildList buildIds buildDatabase = foldM build' buildDatabase buildIds
  where build' a b = build b a -- Build needs an argument swap to work with foldM

--Build all builds in the build database 
buildAll :: BuildDatabase -> IO BuildDatabase
buildAll buildDatabase = foldM build' buildDatabase $ zip [1..] buildIds
  where build' db (index, buildId) = let packageName = package . fromJust . Map.lookup buildId $ idMap buildDatabase
                                     in do putStrLn $ format "{0}/{1} - {2}" [show index, totalBuilds, packageName]
                                           build buildId db -- Build needs an argument swap to work with foldM
        buildIds = Map.keys $ idMap buildDatabase
        totalBuilds = show $ length buildIds

--Helper function add a build result to the database
addBuildResult :: BuildDatabase ->  BuildId -> BuildResult -> BuildDatabase
addBuildResult buildDatabase buildId buildResult = buildDatabase {resultMap = resultMap'}
  where resultMap' = Map.insert buildId buildResult $ resultMap buildDatabase



--Helper functions for constructing a BuildData  
--Filter out only those elements of the context which are dependencies of this package
getDependenciesFromContext :: (PackageDatabase db) => db -> Context -> PackageName -> [PackageName]
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

--Return the result associated to the given buildId
-- Use a default of not build if not in the resultMap.
getBuildResultFromId :: BuildDatabase -> BuildId -> BuildResult
getBuildResultFromId database buildId = Map.findWithDefault NotBuilt buildId $ resultMap database  