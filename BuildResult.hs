{-# LANGUAGE TemplateHaskell #-}
module BuildResult(BuildResult(..)) where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

--Define build result here.

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail          -- Build failed in progress of building this package.
                 | NotBuilt           -- The package has not yet been built
                 deriving(Eq,Show,Read,Ord,Enum)

--Derive a persistance field for the build result
derivePersistField "BuildResult"