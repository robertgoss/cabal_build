module Build(BuildDatabase(..),BuildData(..),BuildResult(..),BuildId,
             addAllPrimaryBuildData,addPrimaryBuildData,
             addLatestPrimaryBuildData,
             build,
             buildAll) where

import Package
import BuildResult
import qualified System

import Data.Hash
import Data.Maybe(fromJust,isNothing,isJust,mapMaybe,listToMaybe)
import Control.Monad(foldM,liftM)
import Text.Format(format)
import Data.List(sort,intersperse)
import Data.Word(Word64)

import Data.Conduit
import qualified Data.Set as S
import qualified Data.Conduit.List as CL

import qualified Data.Text as T

import Debug.Trace(trace)

type BuildId = Word64


data BuildData = BuildData { 
                             package :: PackageName,
                             packageDependencies :: Maybe [PackageName], --Maybe represents a potential failure in resolution
                             buildDependencies :: Maybe [BuildId]
                           }

instance Show BuildData where
    show buildData | isNothing $ packageDependencies buildData = show (package buildData) ++ " *resolution failed*"
                   | otherwise = concat $ show (package buildData) : " $ " : (intersperse " " . map show . fromJust . packageDependencies $ buildData)


--All the other packages which will be installed with a dependency.
-- Also adds the packagenames of the installed packages on the system.
type Context = S.Set PackageName

--Build dependencies should not count towards hash as equality should be determined by packacge and package dependencies alone
getId :: BuildData -> BuildId
getId buildData = asWord64 $ package' `combine` packageDependencies'
    where package' = hash $ package buildData
          --Sort this  as ordering should not matter for equality.
          packageDependencies' = hash . fmap sort $  packageDependencies buildData



data Build = Build BuildData BuildResult
            
--An abstraction of the features of a package database held in memory
class BuildDatabase db where
  emptyBuildDatabase :: IO db

  idExists :: db -> BuildId -> IO Bool

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
fromPackageDatabase packageDatabase = do buildDb <- emptyBuildDatabase
                                         addAllPrimaryBuildData packageDatabase buildDb
                                         buildAll buildDb
                                         return buildDb

--Sub contructors used internally in this module to construct the package database from the system

--Add the buildData for all the packages in the packageDatabase as primary builds to the 
-- build database use a fold to update the database over the packagelist
-- Perform fold in conduit to reduce amount loaded into memory
addAllPrimaryBuildData :: (PackageDatabase db, BuildDatabase buildDb) => db -> buildDb -> IO ()
addAllPrimaryBuildData packageDatabase buildDatabase = do nameSource <- packageNameSource packageDatabase
                                                          nameSource $$ CL.mapM_ addPrimaryIter -- Fuse together source and fold
    where addPrimaryIter currPackage = do putStrLn (show currPackage)
                                          addPrimaryBuildData currPackage packageDatabase buildDatabase

--Add the buildData for all latest the packages in the packageDatabase as primary builds to the 
-- build database use a fold to update the database over the packagelist
-- Perform fold in conduit to reduce amount loaded into memory
addLatestPrimaryBuildData :: (PackageDatabase db, BuildDatabase buildDb) => db -> buildDb -> IO ()
addLatestPrimaryBuildData packageDatabase buildDatabase = do packageSource <- latestPackageSource packageDatabase
                                                             packageSource $$ CL.mapM_ addPrimaryIter -- Fuse together source and fold
    where addPrimaryIter currPackage = do putStrLn (show currPackage)
                                          addPrimaryBuildData currPackage packageDatabase buildDatabase


--Add the buildData of the following primary build and it's dependencies to the database
addPrimaryBuildData :: (PackageDatabase db,BuildDatabase buildDb) 
                       => PackageName -> db -> buildDb -> IO ()
addPrimaryBuildData packageName pkDb buildDb = do --First get the specific dependency context to build this in
                                                  depends' <- System.dependencies (show packageName)
                                                  --See if a dependence error was raise in which case add a stub else 
                                                  -- Add a non primary with that context
                                                  buildId <- case depends' of
                                                               Left _ -> addResolutionFailedBuildData packageName buildDb
                                                               Right depends -> addNonPrimaryBuildData' pkDb buildDb (makeContext depends) packageName
                                                  --Add to primary list
                                                  addPrimary packageName buildId buildDb
                                                  return ()
    where makeContext = S.fromList . map fromString
          --Fiddle addNonPrimaryBuildData to only return the buildId and drop context
          addNonPrimaryBuildData' p b c n = do res <- addNonPrimaryBuildData p b c n
                                               return $ fst res
 
--Add the build data of the follwing package o the database where it has failed to resolve
--Return buildId of build
addResolutionFailedBuildData :: (BuildDatabase buildDb) => PackageName -> buildDb -> IO BuildId
addResolutionFailedBuildData packageName buildDb = do addId buildId buildData buildDb
                                                      addResult buildId ResolutionFailure buildDb
                                                      return buildId
      where buildId = getId buildData
            --Stub buildData
            buildData = BuildData packageName Nothing Nothing

--Add the buildData of the build of the given package in context and it's dependencies to the database
-- We also return the context that this build is built in.
-- Note the recursion in this is highly inefficient with the bottom layer packages being called 
-- repeatedly by ones with greater dependencies. 
addNonPrimaryBuildData :: (PackageDatabase db,BuildDatabase buildDb) 
                         => db -> buildDb -> Context -> PackageName -> IO (BuildId,Context) 
addNonPrimaryBuildData pkDb buildDb context packageName = do package <- trace ("AddNonPrimary" ++ show packageName) $ liftM fromJust $ getPackage pkDb packageName
                                                             --Add each of the pure dependencies in context 
                                                             let pureDepends = pureDependencies package
                                                                 --Get the name of the packages which make up the pure depends
                                                                 --Igonre all pure depends that are installed or virtual
                                                                 pureDependsNames = mapMaybe (getNameFromContext context) $ S.toList pureDepends
                                                             pureDependResults <- mapM (addNonPrimaryBuildData pkDb buildDb context) pureDependsNames
                                                             let pureDependIds = map fst pureDependResults -- BuildIds of each pure build.
                                                                 pureDependContexts = map snd pureDependResults -- Contexts for each dep build
                                                                 --We take the union of the contexts in each individual dependent
                                                                 -- to get the full list context for this package also add the dependent package names
                                                                 -- as their adding will not neccesarrily add them.
                                                                 packageContext = (S.fromList pureDependsNames) `S.union` (S.unions pureDependContexts)
                                                                 packageDependencies = S.toList packageContext
                                                             --Get all of the build dependencies of the pure dependencies 
                                                             pureDependsData <- mapM (getData buildDb) pureDependIds
                                                             let pureDependBuildDeps = map (fromJust . buildDependencies) pureDependsData
                                                                 --Combine to get the full set of build dependencies
                                                                 packageBuildDependencies = combineDependencies $ pureDependIds:pureDependBuildDeps
                                                                 --Construct build data
                                                                 buildData = BuildData packageName (Just packageDependencies) (Just packageBuildDependencies)
                                                                 --Get build id for this build
                                                                 buildId = getId buildData
                                                             --Add the data to the database and return
                                                             addId buildId buildData buildDb 
                                                             return (buildId, packageContext)
                                                                 
   where --Wrap in a maybe as if a package is installed it will not be in the context
         --In this case return nothing
         getNameFromContext context pType = listToMaybe . S.toList $ S.filter (isType pType) context
         isType pType (PackageName nameType _) = pType == nameType
         combineDependencies :: [[BuildId]] -> [BuildId]
         combineDependencies deps = S.toList . S.unions $ map (S.fromList) deps








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
                                                         buildOnSystem
                                                      else 
                                                         addResult buildId DependentFailure database

    where --The Ids of the depenendences use from just as have guarded aganinst resolution failure
          dependenceIds = fromJust $ buildDependencies bData
          dependencePkgs = fromJust $ packageDependencies bData
          --Package name
          packageName = package $ bData
          --Build package on the system and return the result
          -- Register the (now built) dependencies then build package
          buildOnSystem = do mapM_ (registerBuildOnSystem database)  dependenceIds
                             buildStatus <- System.build (show packageName) buildId
                             case buildStatus of
                                --No errors build was successful
                                System.BuildSuccess -> addResult buildId BuildSuccess database
                                --The std err and std out of a failed configure
                                System.ConfigureError outText errText -> addResult buildId (ConfigFail outText errText) database
                                --The std err and std out of a failed build
                                System.BuildError outText errText -> addResult buildId (BuildFail outText errText) database


--Registers a build with the system use the given packageName and differentiate it with the buildid
registerBuildOnSystem :: (BuildDatabase db) => db ->  BuildId -> IO ()
registerBuildOnSystem db buildId = do bData <- getData db buildId
                                      let packageName = package $ bData
                                          depIds = fromJust . buildDependencies $ bData -- as has been build deps exist
                                      --Register depenencies first
                                      mapM_ (registerBuildOnSystem db) depIds 
                                      System.register (show packageName) buildId

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
                                                    putStrLn $ format "{0}/{1} - {2}" [show index, totalBuilds,show (package buildData)]
                                                    build buildId db -- Build needs an argument swap to work with foldM
        indices = [1..] :: [Int]






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
