module Package(PackageType,PackageVersion,
               PackageName(..), sameType, 
               Package(..),
               PackageDatabase(..),
               createPackageDatabase
    ) where

import Data.List.Split(splitOn)
import Data.List(intersperse)

import Data.Conduit

type PackageType = String
type PackageVersion = [Int]




data PackageName = PackageName PackageType PackageVersion
  deriving(Eq,Ord)

instance Show PackageName where
  show (PackageName pType pVersion) = concat . intersperse "-" $ pType : map show pVersion


--Construct a packageName from a string
fromString :: String -> PackageName
fromString string = PackageName pType pVersion
  -- The type is all the string before the last - the version is the rest
  where parts = splitOn "-" string
        pType = concat . intersperse "-" $ init parts
        pVersion = map read . splitOn "." $ last parts


--If 2 package names are of the same type (with potentially different versions)
sameType :: PackageName -> PackageName -> Bool
sameType (PackageName type1 _) (PackageName type2 _) = type1 == type2





data Package = Package {
  name :: PackageName,
  pureDependencies :: [PackageType]
} 


class PackageDatabase db where
  emptyDatabase :: IO db

  addPackage :: Package -> db -> IO () 
  getPackage :: db -> PackageName -> IO (Maybe Package)

  packageNameSource :: db -> IO (Source IO PackageName)

  fetchedSource :: db -> IO (Source IO PackageName)
  unfetchedSource :: db -> IO (Source IO PackageName)
  fetched :: PackageName -> db -> IO () 

packageFromSystem :: PackageName -> IO Package
packageFromSystem = undefined

createPackageDatabase :: (PackageDatabase db) => IO db 
createPackageDatabase = undefined


convertBackend :: (PackageDatabase db, PackageDatabase db') => db -> IO db'
convertBackend = undefined 