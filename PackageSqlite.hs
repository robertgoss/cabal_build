{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module PackageSqlite(PackageDatabaseSqlite) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Conduit
import Data.Conduit.List

import qualified Package 

type VersionString = String

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
PackageSqlite
    name Package.PackageName --Store the full package name
    dependencies [Package.PackageName] Maybe --PackageDependencies but mkPersist need alternative def.
    UniqueName name -- The name should be unique.
    deriving Show
|]


--We do not expose this constructor so the only availible constructor creates an empty database
data PackageDatabaseSqlite = PackageDatabaseSqlite


--Convert PackageDatabaseSqlite to be a packagedatabase
-- Here we use a constant path to the database for now.
instance Package.PackageDatabase PackageDatabaseSqlite where
    emptyDatabase = runSqlite "package-sqlite.data" $ do runMigration migrateAll
                                                         return PackageDatabaseSqlite
    keys _ = runSqlite "package-sqlite.data" $ keysQuery
    insert name deps _ = runSqlite "package-sqlite.data" $ do insertQuery name deps
                                                              return PackageDatabaseSqlite
    getDependency _ name = runSqlite "package-sqlite.data" $ dependenceQuery name

--Required queries
--Get database keys - no filter only return name
-- Use an conduit and filte the list as go so dont need all in memory
keysQuery =  selectSource [] [] $$ Data.Conduit.List.map onlyName =$ consume
    where onlyName = packageSqliteName . entityVal

insertQuery name deps = insert $ PackageSqlite name deps

dependenceQuery name = do package <- getBy $ UniqueName name
                          return . fmap (packageSqliteDependencies . entityVal) $ package



