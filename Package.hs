module Package(PackageName, PackageDependencies, Package(..), PackageDatabase(..),
               packageName,differentVersions,
               fromSystem,
               packageList, getPackage,
              ) where

import qualified System
import Data.List(intersperse)
import Data.List.Split(splitOn)
import Text.Format(format)
import Control.Monad(forM)
import Data.Maybe(fromJust)

type PackageName = String
type PackageDependencies = Maybe [PackageName] -- Wrapped in a maybe to indicate if dependencies could not be resolved.

data Package = Package { name :: String,
                         version :: [Int],
                         dependencies :: PackageDependencies
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
differentVersions :: PackageName -> PackageName -> Bool
differentVersions  pName1 pName2 = name1 == name2
  where (name1,_) = splitPackageName pName1
        (name2,_) = splitPackageName pName2

--An abstrction of the features of a package database used in memeory
class PackageDatabase a where
  emptyDatabase :: a
  keys :: a -> [PackageName]
  insert :: PackageName -> PackageDependencies -> a -> a
  getDependency :: a -> PackageName -> Maybe PackageDependencies


--Basic constructors of the full package database - either from a file or directly computed from system.
--  These are the only constructors to be exported from this module so no partially constructed databases are made.
-- Get the list of packagenames from the system and then iterativly add then to the system
-- Calling system to get the dependance for the package
fromSystem :: (PackageDatabase db) => IO db
fromSystem = do System.cleanCabalSystem -- Clean system so there are no local packages to mess dependencies.
                putStrLn "Loading package list..."
                packageNames <- packageListFromSystem
                let totalPackages = show $ length packageNames
                --Get the packages using packageFromSystem we add an enumeration to
                -- Allow us to trace the process as it is very slow.
                packages <- forM (zip indices packageNames) $ \(index,name) ->
                                             do putStrLn $ format "{0}/{1} - {2}" [show index, totalPackages, name]
                                                packageFromSystem name
                putStrLn "Compiling database..."
                return $ foldl addPackage' emptyDatabase packages -- Swap arguments to addPackage to work with foldl                                                       
  where addPackage' a b = addPackage b a
        indices = [1..] :: [Int]


--Sub contructors used internally in this module to construct the package database from the system

--  Add package to the database
addPackage :: (PackageDatabase db) => Package -> db -> db
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
packageDependenciesFromSystem :: PackageName -> IO PackageDependencies 
packageDependenciesFromSystem = System.dependencies

--  Get the list of all availible packages from the system
packageListFromSystem :: IO [PackageName]
packageListFromSystem = System.packageList


--Properties of the package database that can be extracted

--The list of available packages
--Uses the keys of the dependence map to get list of packageNames
packageList :: (PackageDatabase db) => db -> [Package]
packageList database = map (getPackage database) names
    where names = keys database

--Get package from the database from it's package name
-- Is assumed to be in database
getPackage :: (PackageDatabase db) => db -> PackageName -> Package
getPackage database name = Package { name = packageName,
                                     version = packageVersion,
                                     dependencies = packageDependencies
                                   }
    where packageDependencies = fromJust $ getDependency database name
          (packageName, packageVersion) = splitPackageName name
