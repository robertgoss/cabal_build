module Package(PackageName, PackageDependencies, Package(..), PackageDatabase(..),
               packageName,differentVersions,splitPackageName,
               fromSystem,
               convertBackend,
               packageList, getPackage,
              ) where

import qualified System
import Data.List(intersperse)
import Data.List.Split(splitOn)
import Text.Format(format)
import Control.Monad(forM, foldM)
import Data.Maybe(fromJust)

import Data.Conduit
import qualified Data.Conduit.List as CL

type PackageName = String
type PackageDependencies = Maybe [PackageName] -- Wrapped in a maybe to indicate if dependencies could not be resolved.

data Package = Package { name :: String,
                         version :: [Int],
                         dependencies :: PackageDependencies,
                         installed :: Bool -- Bool to see if this package is already installed.
                       } deriving(Show)

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
  packageNameSource :: a -> IO (Source IO PackageName)
  insert :: PackageName -> Bool -> PackageDependencies -> a -> IO a -- Bool indicating if package is installed.
  isInstalled :: a -> PackageName -> IO (Maybe Bool)
  getDependency :: a -> PackageName -> IO (Maybe PackageDependencies)

--Convert data between backends
convertBackend :: (PackageDatabase a, PackageDatabase b) => a -> IO b
convertBackend database = do empty <- emptyDatabase
                             nameSource <- packageNameSource database
                             nameSource $$ CL.foldM movePackage empty -- Fold over the source of names
    where movePackage db name = do putStrLn name -- Adding a trace
                                   pkg <- getPackage database name
                                   let pName = packageName pkg
                                       deps = dependencies pkg
                                       inst = installed pkg
                                   insert pName inst deps db


--Basic constructors of the full package database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
-- Get the list of packagenames from the system and then iterativly add then to the system
-- Calling system to get the dependance for the package
fromSystem :: (PackageDatabase db) => IO db
fromSystem = do System.cleanCabalSystem -- Clean system so there are no local packages to mess dependencies.
                putStrLn "Create empty database"
                empty <- emptyDatabase
                putStrLn "Loading package list..."
                packageNames <- packageListFromSystem
                let totalPackages = show $ length packageNames
                --Get the packages using packageFromSystem we add an enumeration to
                -- Allow us to trace the process as it is very slow.
                -- Then add the package to the database
                -- Use a fold to keep the database updated
                foldM (getAndAddPackage totalPackages) empty $ zip indices packageNames                                          
  where getAndAddPackage total db (index, name) = do putStrLn $ format "{0}/{1} - {2}" [show index, total, name] --Trace a line.
                                                     package <- packageFromSystem name -- Get the packae information from the system.
                                                     addPackage package db -- Add package to the database and return the new database.
        indices = [1..] :: [Int]


--Sub contructors used internally in this module to construct the package database from the system

--  Add package to the database
addPackage :: (PackageDatabase db) => Package -> db -> IO db
addPackage package database = insert name pInstalled pDependencies database
    where name = packageName package
          pDependencies = dependencies package
          pInstalled = installed package

--  Construct a package from it's name by querying the system
packageFromSystem :: PackageName -> IO Package
packageFromSystem package = do (installed, packageDependencies) <- packageDependenciesFromSystem package
                               return Package { name = packageName ,
                                                version = packageVersion,
                                                dependencies = packageDependencies,
                                                installed = installed
                                              } 
    where (packageName, packageVersion) = splitPackageName package
 
--  Get the dependencies for a given package by querying the system
-- Also returns if the package is installed
  --Convert the either returned by system into if the package has been installed and a maybe on dependencies
packageDependenciesFromSystem :: PackageName -> IO (Bool,PackageDependencies) 
packageDependenciesFromSystem name = do depEither <- System.dependencies name
                                        return $ case depEither of
                                                    --Full success
                                                    Right deps -> (False, Just deps)
                                                    Left System.ReinstallsNeeded -> (False,Nothing) 
                                                    -- Treat reinstalls as a resolution failure
                                                    Left System.ResolutionFail -> (False, Nothing)
                                                    Left System.PackageInstalled -> (True, Nothing)

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

--Get package from the database from it's package name
-- Is assumed to be in database
getPackage :: (PackageDatabase db) => db -> PackageName -> IO Package
getPackage database name = do packageDependencies <- fmap fromJust $ getDependency database name
                              installed <- fmap fromJust $ isInstalled database name
                              let (packageName, packageVersion) = splitPackageName name
                              return $ Package { name = packageName,
                                                 version = packageVersion,
                                                 dependencies = packageDependencies,
                                                 installed = installed
                                               }
