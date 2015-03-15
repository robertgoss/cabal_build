module PackageMemory(PackageDatabaseMemory, saveDatabase, loadDatabase) where

import qualified Data.Map as Map
import Data.ByteString as BS(writeFile, readFile)
import Data.Serialize(encode,decode)

import Package
import qualified Data.Conduit.List as CL

--A package database held completely in memory
newtype PackageDatabaseMemory = PackageDatabaseMemory (Map.Map PackageName (Bool,PackageDependencies))

instance PackageDatabase PackageDatabaseMemory where
  emptyDatabase = return $ PackageDatabaseMemory Map.empty
  packageNameSource (PackageDatabaseMemory depMap) = return . CL.sourceList $ Map.keys depMap
  insert name inst dep (PackageDatabaseMemory depMap) = return $ PackageDatabaseMemory $ Map.insert name (inst, dep) depMap
  getDependency (PackageDatabaseMemory depMap) name = return . fmap snd $ Map.lookup name depMap
  isInstalled (PackageDatabaseMemory depMap) name = return . fmap fst $ Map.lookup name depMap


--Load / Save database to a file

saveDatabase :: PackageDatabaseMemory -> IO ()
saveDatabase (PackageDatabaseMemory database) = BS.writeFile "package.data" $ encode database

--Note will error if cannot decode database - unsafe!!
loadDatabase :: IO PackageDatabaseMemory
loadDatabase = do bytestring <- BS.readFile "package.data"
                  case (decode bytestring) of
                      (Right database) -> return (PackageDatabaseMemory database)