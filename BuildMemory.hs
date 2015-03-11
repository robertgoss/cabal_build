module BuildMemory(BuildDatabaseMemory, 
	               loadBuildDatabase, saveBuildDatabase) where

import qualified Data.Map as Map
import Data.ByteString as BS(writeFile, readFile)
import Data.Serialize(encode,decode)

import Package
import Build

 --A completely in memory database using maps - can become impractical
data BuildDatabaseMemory = BuildDatabaseMemory {
                                       resultMap  :: (Map.Map BuildId BuildResult),-- Map of a build data to the result.
                                       primaryMap :: (Map.Map PackageName BuildId),-- Map of package name to the primary build associated to it
                                       idMap ::      (Map.Map BuildId BuildData)     -- Map of a buildId to it's associated build data
                                     }



--Make an instance of build database
instance BuildDatabase BuildDatabaseMemory where
  emptyBuildDatabase = return $ BuildDatabaseMemory Map.empty Map.empty Map.empty

  addId buildId buildData database = return $ database {idMap = idMap'}
    where idMap' = Map.insert buildId buildData $ idMap database
  addResult buildId buildResult database = return $ database {resultMap = resultMap'}
    where resultMap' = Map.insert buildId buildResult $ resultMap database
  addPrimary packageName buildId database = return $ database {primaryMap = primaryMap'}
    where primaryMap' = Map.insert packageName buildId $ primaryMap database

  getData database buildId = return $ fromJust . Map.lookup buildId $ idMap database
  getResult database buildId = return $ Map.findWithDefault NotBuilt buildId $ resultMap database
  allIds database = return $ Map.keys $ idMap database 




--Basic IO to load and store the build databases as files. Uses cereal - not sure of 
-- Stabiliy on data change, used to store results of long build proccess

--Save the build database
-- Encode the build database as a tuple of maps as cereal can serials tuples and maps
-- Also we save the build results map with the results as integers as cereal can decode these and buildResults is an Enum
-- Also encode builddata as tuple.
-- Finally as Hash is not serializable save the associated word64
saveBuildDatabase :: BuildDatabaseMemory -> IO ()
saveBuildDatabase buildDatabase = BS.writeFile "build.data" $ encode buildTuple
    where buildTuple = (resultMapInt, primaryMapWord, idMapTuple)
          -- Map the various field of the maps to the serializable equivilents.
          resultMapInt = Map.mapKeys asWord64 . Map.map fromEnum $ resultMap buildDatabase
          toDataTuple (BuildData a b c) = (a,b,fmap (map asWord64) c)  -- Convert hash list
          primaryMapWord = Map.map asWord64 $ primaryMap buildDatabase
          idMapTuple = Map.mapKeys asWord64 . Map.map toDataTuple $ idMap buildDatabase

--Load database
-- The database is encoded as a tuple of the maps in build see save.
-- The resultMap has buildresuts encoded as ints
--This throws an error if it cannot decode the database.
loadBuildDatabase :: IO BuildDatabaseMemory
loadBuildDatabase = do bytestring <- BS.readFile "build.data"
                       case (decode bytestring) of
                         (Right buildTuple) -> return (fromBuildTuple buildTuple)
  where fromBuildTuple :: (Map.Map Word64 Int, Map.Map String Word64, Map.Map Word64 (String,Maybe [String],Maybe [Word64])) -> BuildDatabaseMemory
        fromBuildTuple (resultMapInt, primaryMapWord, idMapTuple) = BuildDatabaseMemory resultMap primaryMap idMap 
           where resultMap = Map.mapKeys hashReverse . Map.map toEnum $ resultMapInt
                 idMap = Map.mapKeys hashReverse . Map.map fromDataTuple $ idMapTuple
                 primaryMap = Map.map hashReverse primaryMapWord
                 -- This function reverses the asWord64 used in save by for a given word64 looking up the buildData and taking its hash
                 hashReverse word = getId . fromDataTuple .fromJust $ Map.lookup word idMapTuple 
                 fromDataTuple (a,b,c) = BuildData a b (fmap (map hashReverse) c) -- Convert tuple back to build data. Unconvert asWord
