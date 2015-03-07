module Package(PackageName, PackageDependencies, Package, PackageDatabase(),
               packageName,
               fromSystem,
               packageList, getPackage, getVersions
              ) where

import qualified System
import qualified Data.Map as Map
import Data.List(intersperse)
import Data.List.Split(splitOn)

type PackageName = String
type PackageDependencies = Maybe [PackageName] -- Wrapped in a maybe to indicate if dependencies could not be resolved.

data Package = Package { name :: String,
                         version :: [Int],
                         dependencies :: PackageDependencies
                       }

--Properties of package
--Get the full package nae of a package. This is in the form name-version. This will
-- be used as an internal reference to the package in the package database below.
packageName :: Package -> PackageName 
packageName package = concat $ name package : "-" : map show (version package)
 

--Split a package name into the name of the package and it's version number
splitPackageName :: PackageName -> (String, [Int])
splitPackageName packageName = (name , version)
    --The name is everything before the last - the version is everything after
    where parts = splitOn "-" packageName
          nameParts = init parts
          verPart = last parts
          name = concat $ intersperse "-" nameParts
          version = map read $ splitOn "." verPart

newtype PackageDatabase = PackageDatabase (Map.Map PackageName PackageDependencies)


--Basic constructors of the full package database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
fromSystem :: IO PackageDatabase
fromSystem = undefined

--Sub contructors used internally in this module to construct the package database from the system

--  Add package to the database
addPackage :: Package -> PackageDatabase -> PackageDatabase
addPackage = undefined

--  Construct a package from it's name by querying the system
packageFromSystem :: PackageName -> IO Package
packageFromSystem package = do packageDependencies <- packageDependenciesFromSystem package
                               return Package { name = packageName ,
                                                version = packageVersion,
                                                dependencies = packageDependencies
                                              } 
    where (packageName, packageVersion) = splitPackageName package

--  Get the dependencies for a given package by querying the system
packageDependenciesFromSystem :: PackageName -> IO PackageDependencies 
packageDependenciesFromSystem = System.dependencies

--  Get the list of all availible packages from the system
packageListFromSystem :: IO [PackageName]
packageListFromSystem = System.packageList


--Properties of the package database that can be extracted

--The list of available packages
packageList :: PackageDatabase -> [Package]
packageList = undefined

--Get package from the database from it's package name
getPackage :: PackageDatabase -> PackageName -> Package
getPackage = undefined

--Get all versions of a given package in the database
getVersions :: PackageDatabase -> Package -> [Package]
getVersions = undefined

