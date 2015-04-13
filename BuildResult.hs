{-# LANGUAGE TemplateHaskell #-}
module BuildResult(BuildResult(..)) where

import Database.Persist
import Database.Persist.TH

import qualified Data.Text as T

--Define build result here.

data BuildResult = BuildSuccess       -- Build complete success.
                 | ResolutionFailure  -- Failed to resolve the dependencies of this package.
                 | DependentFailure   -- A package build that this package depends on failed to build.
                 | BuildFail T.Text T.Text        -- Build failed in progress of building this package.
                                            -- Include the stderr and stdout of the build in th case.
                 | ConfigFail T.Text T.Text        -- Build failed in progress of configuring this package.
                                            -- Include the stderr and stdout of the build in th case.
                 | NotBuilt           -- The package has not yet been built
                 deriving(Eq,Show,Read,Ord)

--Derive a persistance field for the build result
derivePersistField "BuildResult"