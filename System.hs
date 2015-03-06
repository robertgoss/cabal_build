module System(packageList,dependencies,build) where

--File for direct (lowlevel) interface with cabal commands

--Get the list of names of all currently available packages to the cabal system
packageList :: IO [String]


--Given the current package-name find the dependent packages that would need
-- to be installed. This is wrapped in a maybe to detect a failure in resolution.
dependencies :: String -> IO (Maybe [String])



--Build the current list of packages return the success or failure of the build.
build :: [String] -> IO Bool


-- Internal commands to help manage the system state.

--Clean the cabal local state.
-- Uninstalls all the local packages - by nuking the local package lists and cabal local builds
-- This is inefficient but the easiest way to start off.
cleanCabalSystem :: IO ()
