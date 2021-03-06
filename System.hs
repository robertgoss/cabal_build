{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System(packageList,
              pureDependencies,
              dependencies,fetch,build,register,
	            cleanCabalSystem,
             DepError(..),BuildStatus(..)) where

--Import of shelly library - it defaults to using Text instead of string
--  This reduces coercion for literals by default to Text.
import Shelly
import Control.Monad(liftM)
import Data.Word
import qualified Data.Text as T
default (T.Text)

--File for direct (lowlevel) interface with cabal commands

--Get the list of names of all currently available packages to the cabal system
packageList :: IO [String]
packageList = shelly . silently . liftM (map T.unpack) $ packageListSh

--Run the list command and split the resultant output into lines
-- We do a replace to change the output format to the standard packagename format.
packageListSh :: Sh [T.Text]
packageListSh = liftM processOutput $ run "cabal" ["list","--simple"]
	where processOutput = T.lines . T.replace " " "-"

--A data structure to encode the different ways the dependencies function can fail.
data DepError = ResolutionFail
                | PackageInstalled
                | ReinstallsNeeded
                | OverBackjump
                deriving(Eq,Show)
--Given the current package-name find the dependent packages that would need
-- to be installed. This is wrapped in a either to detect a failure in resolution or if
-- the package is already installed.
-- We unpack the inner text into a string.
dependencies :: String -> IO (Either DepError [String])
dependencies = shelly . silently . liftM (fmap (map T.unpack)) . dependenciesSh


--Get dependencies by performing a dry-run installation.
--Use errExit false as we will use lastExitCode to see if it failed then wrap result in a either
dependenciesSh :: String -> Sh (Either DepError [T.Text])
dependenciesSh name = errExit False $ do depText <- run "cabal" ["install", package , "--dry-run"]
                                         exitCode <- lastExitCode
                                         stdErr <- lastStderr
                                         let installed = isInstalled depText
                                             reinstalls = needReinstalled stdErr -- Reinstalls error is prnted to stderr!
                                             backjump = overBackjump stdErr -- Overjump err is printed to stderr
                                         --Package has a resolution failed and cant be installed 
                                         -- If the dry-run failes or if it is already installed.
                                         if (exitCode == 0) then
                                             if (not installed) then
                                                if (not reinstalls)
                                                          then return . Right $ processOutput depText
                                                          else return $ Left ReinstallsNeeded
                                         	      else return $ Left PackageInstalled
                                             else --See if resolution failed as this packag cant be resolved or if system 
                                                  -- Couldnt find one given resouces
                                                  if backjump then return $ Left OverBackjump
                                                  else return $ Left ResolutionFail
    where package = T.pack name
          --Process the output of the dry run into the dependencies
          --Split into lines, drop the first 2 lines which are boilerplate
          -- drop the last line which is this package
          --On each line only take upto the first space - to ignore the lastest
          -- information given on some lines
          processOutput = map dropLatest . drop 2 . init . T.lines
          dropLatest = head . T.splitOn " "
          --See if this package is installed already and so will propt a build failure
          -- check the second line of the out put to see if it is the same as some 
          -- given text
          isInstalled output = (T.lines output) !! 1 == "All the requested packages are already installed:"
          --See if reinstalls of (system) packages are needed
          -- For now we will treat this as a plain build failure 
          -- Check last line for given text.
          needReinstalled output 
             | T.null output = False -- guard against empty sterr
             | otherwise = last (T.lines output) == "Use --force-reinstalls if you want to install anyway."
          --See if we failed to find a resolution as we used up the quota of backjumps
          --Check last line for given text
          overBackjump stderr
             | T.null stderr = False -- guard against empty sterr
             | otherwise = last (T.lines stderr) == "Backjump limit reached (change with --max-backjumps)." 

--Perform a fetch of the given package
fetch :: String -> IO Bool
fetch = shelly . silently . errExit False . fetchSh -- Use exitErr as we will be checking lastExitCode rather than for an exception.

--Call the command and check its success
fetchSh :: String -> Sh Bool
fetchSh name = do run_ "cabal" ["fetch","--no-dependencies",T.pack name]
                  success <- lastExitCode
                  return $ success == 0


--Unpack the build to it's own directory
-- Give the package name and the hash to differentiate it from packages with different contexts
unpack :: String -> Word64 -> IO ()
unpack p h = shelly . silently $ unpackSh p h

--Call the cabal command 
-- The build is placed in the directory $HOME/.cabal-build/hash/
unpackSh :: String -> Word64 -> Sh ()
unpackSh packageName hash = do home_env <- get_env_text "HOME"
                               let home_path = T.replace "/n" "" home_env -- Get file path for the home directory from 
                                                                          -- Env variable. Remove the extra \n
                                   build_path = home_path </> ".cabal-build" </> show hash -- The directory the build will go in
                               mkdir_p $ build_path -- Create directory for build
                               run_ "cabal" ["get", T.pack packageName, "-d" `T.append` toTextIgnore build_path]


--Register a pre-existing, prebuilt package with the local package directory
-- Give the package name and the hash to differentiate it from packages with different contexts
-- The build is placed in the directory $HOME/.cabal-build/hash/
register :: String -> Word64 -> IO ()
register p h = shelly . silently $ registerSh p h


registerSh :: String -> Word64 -> Sh ()
registerSh packageName hash = do home_env <- get_env_text "HOME"
                                 let home_path = T.replace "/n" "" home_env -- Get file path for the home directory from 
                                                                            -- Env variable. Remove the extra \n
                                     build_path = home_path </> ".cabal-build" </> show hash -- The directory the build will go in
                                 cd $ build_path </> packageName --Execute the command in the build directory
                                 run_ "cabal" ["register", "--inplace","--user"] -- Inplace means we do not need to copy to default dir
                                                                                -- Must register to user else will silently fail.
                                 cleanBuildDirectorySh --Remove any setup files which may have to be autogenerated
-- Data structure to show the result status of a build
data BuildStatus = BuildSuccess
                   | ConfigureError T.Text T.Text -- Configure error return the stdout and stderr
                   | BuildError T.Text T.Text -- Build error return the stdout and stderr

--Build the current package that has been previously unpacked
-- Give the package name and the hash to differentiate it from packages with different contexts
-- The build is placed in the directory $HOME/.cabal-build/hash/
build :: String -> Word64 -> IO BuildStatus
build p h = shelly . silently . errExit False $ buildSh p h 
                             -- Use exitErr as we will be checking lastExitCode rather than for an exception.

--Perform the build and check for success - then clean the system bak to it's original pristeen setting
--It is assumed that all dependencies have been met.
buildSh :: String -> Word64 -> Sh BuildStatus
buildSh packageName hash = do home_env <- get_env_text "HOME"
                              let home_path = T.replace "/n" "" home_env -- Get file path for the home directory from 
                                                                         -- Env variable. Remove the extra \n
                                  build_path = home_path </> ".cabal-build" </> show hash -- The directory the build will go in
                              --Unpack the package
                              unpackSh packageName hash
                              cd $ build_path </> packageName --Execute the commands in the build directory
                              --Configure package and get user data
                              config_text <- run "cabal" ["configure", "--user"]
                              config_failure <- fmap (/=0) $ lastExitCode
                              config_error <- lastStderr
                              --Build package
                              build_text <- run "cabal" ["build"]
                              build_failure <- fmap (/=0) $ lastExitCode
                              build_error <- lastStderr
                              --Remove uneeded build files
                              cleanBuildDirectorySh
                              -- Clean the general system back to pristine setting
                              cleanCabalSystemSh
                              --Return result based on failure
                              if config_failure 
                                  then return $ ConfigureError config_text config_error
                                  else if build_failure then return $ BuildError build_text build_error
                                                        else return BuildSuccess -- No failures happened.


--Pure dependencies 
--These are the package types that a given package depends on and are independent between systems
pureDependencies :: String -> IO [String]
pureDependencies = shelly . silently . liftM (map T.unpack) . pureDependenciesSh . T.pack


--Scrape the data using the cabal info command
pureDependenciesSh :: T.Text -> Sh [T.Text]
pureDependenciesSh name = do infoTxt <- run "cabal" ["info", name]
                             --get he lines in info relating to the dependencies
                             --We want the lines between the first appearance of "Dependancy:"
                             -- And the next header - which we can find by looking for the next line containing a colon
                             let infoLines = dropWhile notDependencies $ T.lines infoTxt
                                 --Remove the Dependency: from the first line
                                 -- And on the tail take until hit next colon
                                 -- This must be done on tail as first line contains a colon from Dependencies:
                                 infoLines' = (removeDependencies $ head infoLines) : (takeWhile noColon $ tail infoLines)
                                 --Concat together and split into individual dependants
                                 dependentsRough = T.splitOn "," $ T.concat infoLines'
                                 --For each rough dependant extract the packagetype - it is the first non-empty word
                                 dependentTypes = map (firstNonNull . T.words) dependentsRough
                             --Return dependant types
                             --Deal with the case in which no dependency is specified (Thus infoLines' cant be computed)
                             if null infoLines then return []
                                               else return dependentTypes
    where noColon text = not $ ":" `T.isInfixOf` text
          notDependencies text = not $ "Dependencies:" `T.isInfixOf` text 
          removeDependencies = snd . T.breakOnEnd "Dependencies:"
          firstNonNull = head . dropWhile T.null


-- Internal commands to help manage the system state.

--Clean the cabal local state.
-- Uninstalls all the local packages - by nuking the local package lists and cabal local builds
-- This is inefficient but the easiest way to start off.
cleanCabalSystem :: IO ()
cleanCabalSystem = shelly . silently $ cleanCabalSystemSh


--Clean build dir
--Removes unneeded files from a build directory to save space
-- It is essential that this does not affect the ability to register a package
-- Assumes that we are in the correct directory
cleanBuildDirectorySh :: Sh ()
cleanBuildDirectorySh = do run_ "find" [".","-name","setup","-type","f","-exec","rm","{}",";"] --Remove setup data which remains if build fails
                           --Remove build files which are now unneeded
                           run_ "find" [".","-name","*.o","-exec","rm","{}",";"]
                           run_ "find" [".","-name","*.dyn_o","-exec","rm","{}",";"]


--Remove the ghc director to nuke the package database
--Remove the build files in cabal/lib and cabal/bin
cleanCabalSystemSh :: Sh ()
cleanCabalSystemSh = do home_env <- get_env_text "HOME"
                        let home_path = T.replace "/n" "" home_env -- Get file path for the home directory from 
                                                                   -- Env variable. Remove the extra \n
                        rm_rf $ home_path </> ".ghc"
                        rm_f $ home_path </> "world"
                        rm_rf $ home_path </> ".cabal" </> "bin"
                        rm_rf $ home_path </> ".cabal" </> "lib"
                        rm_rf $ home_path </> ".cabal" </> "share"
                        rm_rf $ home_path </> ".cabal" </> "logs"
                        mkdir $  home_path </> ".cabal" </> "bin"
                        mkdir $  home_path </> ".cabal" </> "lib"
                        mkdir $  home_path </> ".cabal" </> "share"