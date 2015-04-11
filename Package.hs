module Package(PackageType,PackageVersion,
               PackageName(..), sameType, fromString, 
               Package(..),
               PackageDatabase(..),
               createPackageDatabase,
               fetchAll,
               convertBackend
    ) where

import Data.List.Split(splitOn)
import Data.List(intersperse)
import Data.Set(Set,fromList,toList)
import Control.Monad(liftM)
import Data.Maybe(fromJust)

import Text.Format(format)

import Data.Hash
import Data.Conduit
import qualified Data.Conduit.List as CL

import System

type PackageType = String
type PackageVersion = [Int]




data PackageName = PackageName PackageType PackageVersion
  deriving(Eq,Ord)

instance Show PackageName where
  show (PackageName pType pVersion) = pType ++ "-" ++ stringVersion
    where stringVersion = concat . intersperse "." $ map show pVersion

instance Hashable PackageName where
  hash (PackageName pType version) = hash pType `combine` hash version


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
  pureDependencies :: Set PackageType
} 


class PackageDatabase db where
  emptyDatabase :: IO db

  addPackage :: db -> Package -> IO () 
  getPackage :: db -> PackageName -> IO (Maybe Package)

  packageNameSource :: db -> IO (Source IO PackageName)


  fetchedSource :: db -> IO (Source IO PackageName)
  unFetchedSource :: db -> IO (Source IO PackageName)
  fetched :: db -> PackageName  -> IO () 

  getLatest :: db -> PackageType -> IO (Maybe Package)
  makeLatest :: db -> PackageName -> IO ()

  latestPackageSource :: db -> IO (Source IO PackageName)

--Filters out 'fake' packages that are used behind the scenes and which are immutable like ghc and bin-package-db
fakePackageTypes = ["ghc","bin-package-db"]

notFakePackageType pType =  pType `notElem` fakePackageTypes
notFakePackageName (PackageName pType _) = notFakePackageType pType 


--Get a package from system data.
packageFromSystem :: PackageName -> IO Package
packageFromSystem pName = do pureDeps <- System.pureDependencies $ show pName
                             return $ Package pName (fromList $ filter notFakePackageType pureDeps)


--Get package list from system and add for each package get the data from the system and add to database.
createPackageDatabase :: (PackageDatabase db) => IO db 
createPackageDatabase = do database <- emptyDatabase
                           nameList' <- System.packageList 
                           --Convert to package names and filter out fake virtual packages
                           let nameList = filter notFakePackageName $ map fromString nameList'
                           mapM (addName database) $ zip [1..] nameList --Add each of the names to the database 
                           return database
    where addName :: (PackageDatabase db) => db -> (Int,PackageName) -> IO ()
          addName database (i,pName) = do putStrLn $ format "{0} - {1}" [show i,show pName] -- Add a trace to see where we are. 
                                          package <- packageFromSystem pName
                                          addPackage database package
                                          makeLatest database pName  -- This will set the latest package to the current package
                                                                     -- As system returns packages in version order this works overall

--Attempt to fetch all the packages
fetchAll :: (PackageDatabase db) => db -> IO ()
fetchAll database = do unfetched <- unFetchedSource database
                       unfetched $$ CL.mapM_ fetchPackage 
        --Fetch each individual unfetched package if fetch success mark as fetched
    where fetchPackage packageName = do success <- System.fetch $ show packageName
                                        if success then fetched database packageName
                                                   else return ()


convertBackend :: (PackageDatabase db, PackageDatabase db') => db -> IO db'
convertBackend database = do newDatabase <- emptyDatabase
                             --Add all packages from old databse to new database
                             nameSource <- packageNameSource database
                             nameSource $$ CL.mapM_ (movePackage database newDatabase)
                             --Mark all the fetched packages
                             fetchedS <- fetchedSource database
                             fetchedS $$ CL.mapM_ (fetched newDatabase)
                             --Mark all latest package
                             latestSource <- latestPackageSource database
                             latestSource $$ CL.mapM_ (makeLatest newDatabase)
                             --Return
                             return newDatabase
     where movePackage dbOld dbNew name = do package <- liftM fromJust $ getPackage dbOld name --Can use fromjust as fro db.
                                             addPackage dbNew package
