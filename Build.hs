module Build() where

import Package

import Data.Map as Map

data BuildData = BuildData { 
                             dependencies :: [BuildData]
						   } deriving(Eq,Ord)

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail          -- Build failed in progress of building this package.

data Build = Build BuildData BuildResult
            
 --We use a lazy map here so we only need to run a build when we want to query a result
data BuildDatabase = BuildDatabase (Map.Map BuildData BuildResult) -- Map of a build data to the result.
                                   (Map.Map PackageName BuildData) -- Map of package name to the primary build associated to it

--Basic constructors of the full build database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
fromPackageDatabase :: PackageDatabase -> IO BuildDatabase
fromPackageDatabase = undefined

--Sub contructors used internally in this module to construct the package database from the system

--Create an empty build database
emptyBuildDatabase :: BuildDatabase
emptyBuildDatabase = BuildDatabase Map.empty Map.empty

--Add the following build to the build database
addBuild :: Build -> BuildDatabase -> BuildDatabase
addBuild = undefined

--Build the following package on the system
build :: BuildDatabase -> BuildData -> IO BuildResult
build = undefined

--Get the primary build data associated to the pacakge with the given name in the package database.
getBuildData :: PackageName -> PackageDatabase -> BuildData
getBuildData = undefined


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
