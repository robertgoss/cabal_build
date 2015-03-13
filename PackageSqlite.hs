{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module PackageSqlite(loadDatabase, PackageDatabaseSqlite) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe(fromJust)

import qualified Package 

type VersionString = String

--Setup a datatype to store packages as persist objects

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PackageSqlite
    name Package.PackageName -- Allow pulling name list out of database without reconstructing dependencies
    resolutionFailed Bool -- If the resolution failed
    UniqueName name
    deriving Show 
PackageDependence
    package PackageSqliteId
    dependant Package.PackageName
    deriving Show
|]


--We do not expose this constructor so the only availible constructor creates an empty database
--Keep a set of the current packages
data PackageDatabaseSqlite = PackageDatabaseSqlite 

--Mke sure migrations are run in load
loadDatabase :: IO PackageDatabaseSqlite
loadDatabase = runSqlite "package-sqlite.data" $ do runMigration migrateAll
                                                    return PackageDatabaseSqlite

--Convert PackageDatabaseSqlite to be a packagedatabase
-- Here we use a constant path to the database for now.
instance Package.PackageDatabase PackageDatabaseSqlite where
    emptyDatabase = runSqlite "package-sqlite.data" $ do runMigration migrateAll
                                                         return PackageDatabaseSqlite

    --Get a source of package names
    -- Source means they dont all need to be loaded into memory - only id's need to be in memory
    packageNameSource _ = do ids <- runSqlite "package-sqlite.data" $ selectKeysList [] []
                             return $ CL.sourceList ids $= CL.mapM getPackage
          where getPackage pid = runSqlite "package-sqlite.data" . fmap (packageSqliteName . fromJust) $ get pid

    insert name deps _ = runSqlite "package-sqlite.data" $ do insertQuery name deps
                                                              return PackageDatabaseSqlite
    getDependency _ name = runSqlite "package-sqlite.data" $ dependenceQuery name



--Required queries

insertQuery name deps
   | deps == Nothing = insert_ $ PackageSqlite name True -- The resolution has failed do not add more packages
   | otherwise = do pkgId <- insert $ PackageSqlite name False
                    insertMany_ $ zipWith PackageDependence (repeat pkgId) (fromJust deps)

dependenceQuery name = do package' <- getBy $ UniqueName name
                          case package' of 
                            Nothing -> return Nothing -- Could not find the package returning nothing
                            Just package -> if resolutionFailed package then return $ Just Nothing -- Package found but resolution failed
                                            else fmap (Just . Just) $ getDependenceQuery $ entityKey package --Perform the query to get dependencies

    where getDependenceQuery packageId = do dependencies <- selectList [PackageDependencePackage ==. packageId] []
                                            return . Prelude.map (packageDependenceDependant . entityVal) $ dependencies
          resolutionFailed = packageSqliteResolutionFailed . entityVal



