module Package(PackageName, PackageDependencies(..), Package(..), PackageDatabase(..),
               packageName,differentVersions,splitPackageName,
               fromSystem,
               convertBackend,
               packageLatestList, packageLatestSource,
               packageList, packageSource,
               getPackage,
               fetchAll,
               updateLatest
              ) where

import qualified System
import Data.List(intersperse,sort)
import Data.List.Split(splitOn)
import Text.Format(format)
import Control.Monad(forM, foldM)
import Data.Maybe(fromJust)

import Data.Conduit
import qualified Data.Conduit.List as CL

import Debug.Trace(trace)

type PackageName = String
data PackageDependencies =  NotResolved -- The resolution failed and this package cannt be installed on the system
                                        -- Further no package can depend on this one
                                        -- As such it also cannot have dependencies
                            | ResolutionUnknown -- The system wasn't able to determine a resolution for this package
                                                -- One may exist so this package could be a dependant of another
                                                -- As such we cant have dependencies associated to it (yet)
                            | Installed -- This package is installed it has no dependencies as it is functioning
                                        -- As it is installed it's dependencies dont even include itself. 
                            | Dependencies [PackageName] -- Package resolves to have the given list of dependencies
                                                             -- Exclude the package itself from the list of dependencies.
                        deriving(Show)

instance Eq PackageDependencies where
  NotResolved == NotResolved = True
  ResolutionUnknown == ResolutionUnknown = True
  Installed == Installed = True
  (Dependencies dep1) == (Dependencies dep2) = (sort dep1) == (sort dep2) -- Ordering of dpeendencies does not matter
  _ == _ = False

data Package = Package { name :: String,
                         version :: [Int],
                         dependencies :: PackageDependencies
                       } deriving(Show,Eq)

--Properties of package
--Get the full package nae of a package. This is in the form name-version. This will
-- be used as an internal reference to the package in the package database below.
packageName :: Package -> PackageName 
packageName package = concat $ name package : "-" : (intersperse "." . map show . version $ package)
 

--Split a package name into the name of the package and it's version number
splitPackageName :: PackageName -> (String, [Int])
splitPackageName packageName = (name , version)
    --The name is everything before the last - the version is everything after
    where parts = splitOn "-" packageName
          nameParts = init parts
          verPart = last parts
          name = concat $ intersperse "-" nameParts
          version = map read $ splitOn "." verPart

--Return if the 2 given packages are different versions of the same name.
differentVersions :: Package -> PackageName -> Bool
differentVersions  pkg pName = name pkg == name2
  where (name2,_) = splitPackageName pName

--An abstrction of the features of a package database used in memeory
class PackageDatabase a where
  emptyDatabase :: IO a
  --Returns packages in order from most dependencies to least
  packageNameSource :: a -> IO (Source IO PackageName)
  insert :: PackageName -> PackageDependencies -> a -> IO a -- Bool indicating if package is installed.
  getDependency :: a -> PackageName -> IO (Maybe PackageDependencies)
  makeLatest :: PackageName -> a -> IO a
  latest :: a -> String -> IO (Maybe PackageName)
  --Returns in order from most dependenices to least
  latestPackageNameSource :: a -> IO (Source IO PackageName)
  --Gets if a package has been fetched and sets that a package has been fetched
  isFetched  :: a -> PackageName -> IO (Maybe Bool)
  fetched :: PackageName -> a -> IO a

--Convert data between backends
convertBackend :: (PackageDatabase a, PackageDatabase b) => a -> IO b
convertBackend database = do empty <- emptyDatabase
                             nameSource <- packageNameSource database
                             db' <- nameSource $$ CL.foldM movePackage empty -- Fold over the source of names
                             --Fold over the list of latet packages
                             latestNameSource <- latestPackageNameSource db'
                             latestNameSource $$ CL.foldM (flip makeLatest) db' -- Flip arguments
    where movePackage db name = do putStrLn name -- Adding a trace
                                   pkg <- getPackage database name
                                   let pName = packageName pkg
                                       deps = dependencies pkg
                                   insert pName deps db

updateLatest :: (PackageDatabase db) => db -> IO db
updateLatest database = do nameSource <- packageNameSource database
                           nameSource $$ CL.foldM updatePackageLatest database --Fold over each package to update the database
  where updatePackageLatest db packageName = do putStrLn packageName
                                                latest' <- latest db $ name packageName
                                                case latest' of
                                                   Nothing -> makeLatest packageName db -- If this name has no latest yet then add this
                                                   (Just latestName) -> if version packageName > version latestName 
                                                                                then makeLatest packageName db -- This package is later add it as latest
                                                                                else return db -- Current package is latest version

            where name = fst . splitPackageName
                  version = snd . splitPackageName


--Fake package names
-- These are pakage names that ar fake they cant be installed through the cabal system but still appear through
-- The list command
fake_package_names = ["ghc","bin-package-db","rts"]

--Basic constructors of the full package database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
-- Get the list of packagenames from the system and then iterativly add then to the system
-- Calling system to get the dependance for the package
fromSystem :: (PackageDatabase db) => IO db
fromSystem = do System.cleanCabalSystem -- Clean system so there are no local packages to mess dependencies.
                putStrLn "Create empty database"
                empty <- emptyDatabase
                putStrLn "Loading package list..."
                packageNames' <- packageListFromSystem
                let packageNames = filter isFake packageNames' -- Filter out the fake packages that appear in the system list
                    totalPackages = show $ length packageNames
                --Get the packages using packageFromSystem we add an enumeration to
                -- Allow us to trace the process as it is very slow.
                -- Then add the package to the database
                -- Use a fold to keep the database updated
                foldM (getAndAddPackage totalPackages) empty $ zip indices packageNames                                          
  where getAndAddPackage total db (index, name) = do putStrLn $ format "{0}/{1} - {2}" [show index, total, name] --Trace a line.
                                                     package <- packageFromSystem name -- Get the packae information from the system.
                                                     db' <- addPackage package db -- Add package to the database and return the new database.
                                                     makeLatest name db' --Packages appear in version order in the system list.
        indices = [1..] :: [Int]
        isFake name = (fst $ splitPackageName name) `elem` fake_package_names



--Sub contructors used internally in this module to construct the package database from the system

--  Add package to the database
addPackage :: (PackageDatabase db) => Package -> db -> IO db
addPackage package database = insert name pDependencies database
    where name = packageName package
          pDependencies = dependencies package

--  Construct a package from it's name by querying the system
packageFromSystem :: PackageName -> IO Package
packageFromSystem package = do packageDependencies <- packageDependenciesFromSystem package
                               return Package { name = packageName ,
                                                version = packageVersion,
                                                dependencies = packageDependencies
                                              } 
    where (packageName, packageVersion) = splitPackageName package
 
--  Get the dependencies for a given package by querying the system
-- Also returns if the package is installed
  --Convert the either returned by system into if the package has been installed and a maybe on dependencies
packageDependenciesFromSystem :: PackageName -> IO PackageDependencies
packageDependenciesFromSystem name = do depEither <- System.dependencies name
                                        return $ case depEither of
                                                    --Full success
                                                    Right deps -> Dependencies deps
                                                    -- The package cant be resolved on the system
                                                    -- Treat reinstalls as a resolution failure
                                                    Left System.ResolutionFail ->NotResolved
                                                    Left System.ReinstallsNeeded -> NotResolved
                                                    --The package is installed on the system
                                                    Left System.PackageInstalled -> Installed
                                                    --The system failed to resolve this package
                                                    --Due to a timeout / resources 
                                                    -- With further resources a resolution may be found
                                                    Left System.OverBackjump -> ResolutionUnknown

--  Get the list of all availible packages from the system
packageListFromSystem :: IO [PackageName]
packageListFromSystem = System.packageList


--Properties of the package database that can be extracted

--A source of the availible packages much more memory friendly (hopefully than packageList)
packageSource :: (PackageDatabase db) => db -> IO (Source IO Package)
packageSource database = do nameSource <- packageNameSource database
                            return $ nameSource $= CL.mapM (getPackage database)


--The list of available packages
-- As creates all the packages is dangerous in memory!!!
packageList :: (PackageDatabase db) => db -> IO [Package]
packageList database = do pkgSource <- packageSource database
                          pkgSource $$ CL.consume

--A source of the availible packages much more memory friendly (hopefully than packageList)
-- Only returns the latest versions of any package
packageLatestSource :: (PackageDatabase db) => db -> IO (Source IO Package)
packageLatestSource database = do nameSource <- latestPackageNameSource database
                                  return $ nameSource $= CL.mapM (getPackage database)


--Fetch all packages if fetch is successful add a note to the database that it is fetched
fetchAll :: (PackageDatabase db) => db -> IO ()
fetchAll database = do nameSource <- packageNameSource database
                       nameSource $$ CL.mapM_ fetchPackage  --Map fetching package over the names.
    where fetchPackage :: PackageName -> IO ()
          fetchPackage name = do putStrLn name -- Add trace to see where we are
                                 pFetched <- isFetched database name -- See if package already fetched
                                 case pFetched of
                                        Just False ->  do success <- System.fetch name
                                                          if success then markFetched else return () -- If successfully fetched mark in db
                                        otherwise -> return () -- Otherwise move onto the next package
            where markFetched = fetched name database >> return () -- Mark as fetched in database fix return type



--The list of available packages
-- Only returns the latest versions of any package
-- As creates all the packages is dangerous in memory!!!
packageLatestList :: (PackageDatabase db) => db -> IO [Package]
packageLatestList database = do pkgSource <- packageLatestSource database
                                pkgSource $$ CL.consume

--Get package from the database from it's package name
-- Is assumed to be in database
getPackage :: (PackageDatabase db) => db -> PackageName -> IO Package
getPackage database name = do packageDependencies <- getDependency database name
                              let (packageName, packageVersion) = splitPackageName name
                              return $ Package { name = packageName,
                                                 version = packageVersion,
                                                 dependencies = fromJust packageDependencies
                                               }
