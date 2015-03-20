module Package(PackageName, PackageDependencies(..), Package(..), PackageDatabase(..),
               packageName,differentVersions,splitPackageName,
               fromSystem,
               convertBackend,
               packageList, getPackage,
              ) where

import qualified System
import Data.List(intersperse,sort)
import Data.List.Split(splitOn)
import Text.Format(format)
import Control.Monad(forM, foldM)
import Data.Maybe(fromJust)

import Data.Conduit
import qualified Data.Conduit.List as CL

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
  packageNameSource :: a -> IO (Source IO PackageName)
  insert :: PackageName -> PackageDependencies -> a -> IO a -- Bool indicating if package is installed.
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
                                   insert pName deps db


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

--Get package from the database from it's package name
-- Is assumed to be in database
getPackage :: (PackageDatabase db) => db -> PackageName -> IO Package
getPackage database name = do packageDependencies <- getDependency database name
                              let (packageName, packageVersion) = splitPackageName name
                              return $ Package { name = packageName,
                                                 version = packageVersion,
                                                 dependencies = fromJust packageDependencies
                                               }
