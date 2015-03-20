{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System(packageList,dependencies,build,
	           cleanCabalSystem,
             DepError(..)) where

--Import of shelly library - it defaults to using Text instead of string
--  This reduces coercion for literals by default to Text.
import Shelly
import Control.Monad(liftM)
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

--Build the current list of packages return the success or failure of the build.
build :: [String] -> IO Bool
build = shelly . silently . errExit False . buildSh -- Use exitErr as we will be checking lastExitCode rather than for an exception.

--Perform the build and check for success - then clean the system bak to it's original pristeen setting
buildSh :: [String] -> Sh Bool
buildSh names = do run_ "cabal" $ "install" : packages
                   success <- lastExitCode
                   cleanCabalSystemSh --Make sure cleaning is always run
                   return $ success == 0
    where packages = map T.pack names

-- Internal commands to help manage the system state.

--Clean the cabal local state.
-- Uninstalls all the local packages - by nuking the local package lists and cabal local builds
-- This is inefficient but the easiest way to start off.
cleanCabalSystem :: IO ()
cleanCabalSystem = shelly . silently $ cleanCabalSystemSh


--Remove the ghc director to nuke the package database
--Remove the build files in cabal/lib and cabal/bin
cleanCabalSystemSh :: Sh ()
cleanCabalSystemSh = do home_env <- get_env_text "HOME"
                        let home_path = T.replace "/n" "" home_env -- Get file path for the home directory from 
                                                                   -- Env variable. Remove the extra \n
                        rm_rf $ home_path </> ".ghc"
                        rm_rf $ home_path </> ".cabal" </> "bin"
                        rm_rf $ home_path </> ".cabal" </> "lib"
                        rm_rf $ home_path </> ".cabal" </> "share"
                        mkdir $  home_path </> ".cabal" </> "bin"
                        mkdir $  home_path </> ".cabal" </> "lib"
                        mkdir $  home_path </> ".cabal" </> "share"