module Package(PackageName, PackageDependencies, Package, PackageDatabase(),
	           packageName,
	           fromSystem,
	           packageList, getPackage, getVersions
	          ) where

import Data.Map as Map

type PackageName = String
type PackageDependencies = [PackageName]

data Package = Package { name :: String,
		                 version :: [Int],
		                 dependencies :: PackageDependencies
		               }

--Properties of package
--Get the full package nae of a package. This is in the form name-version. This will
-- be used as an internal reference to the package in the package database below.
packageName :: Package -> PackageName 

newtype PackageDatabase = PackageDatabase Map.Map PackageName PackageDependencies


--Basic constructors of the full package database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
fromSystem :: IO PackageDatabase

--Sub contructors used internally in this module to construct the package database from the system

--  Add package to the database
addPackage :: Package -> PackageDatabase -> PackageDatabase

--  Construct a package from it's name by querying the system
packageFromSystem :: PackageName -> IO Package

--  Get the dependencies for a given package by querying the system
packageDependenciesFromSystem :: PackageName -> IO PackageDependencies 

--  Get the list of all availible packages from the system
packageListFromSystem :: IO [PackageName]


--Properties of the package database that can be extracted

--The list of available packages
packageList :: PackageDatabase -> [Package]

--Get package from the database from it's package name
getPackage :: PackageDatabase -> PackageName -> Package

--Get all versions of a given package in the database
getVersions :: PackageDatabase -> Package -> [Package]


