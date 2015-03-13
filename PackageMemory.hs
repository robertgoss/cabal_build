module PackageMemory(PackageDatabaseMemory, saveDatabase, loadDatabase) where

import qualified Data.Map as Map
import Data.ByteString as BS(writeFile, readFile)
import Data.Serialize(encode,decode)

import Package
import qualified Data.Conduit.List as CL

--A package database held completely in memory
newtype PackageDatabaseMemory = PackageDatabaseMemory (Map.Map PackageName PackageDependencies)

instance PackageDatabase PackageDatabaseMemory where
  emptyDatabase = return $ PackageDatabaseMemory Map.empty
  packageNameSource (PackageDatabaseMemory depMap) = CL.sourceList $ Map.keys depMap
  insert name dep (PackageDatabaseMemory depMap) = return $ PackageDatabaseMemory $ Map.insert name dep depMap
  getDependency (PackageDatabaseMemory depMap) name = return $ Map.lookup name depMap


--Load / Save database to a file

saveDatabase :: PackageDatabaseMemory -> IO ()
saveDatabase (PackageDatabaseMemory database) = BS.writeFile "package.data" $ encode database

--Note will error if cannot decode database - unsafe!!
loadDatabase :: IO PackageDatabaseMemory
loadDatabase = do bytestring <- BS.readFile "package.data"
                  case (decode bytestring) of
                      (Right database) -> return (PackageDatabaseMemory database)