{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System(packageList,dependencies,build) where

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

--Given the current package-name find the dependent packages that would need
-- to be installed. This is wrapped in a maybe to detect a failure in resolution.
-- We unpack the inner text into a string.
dependencies :: String -> IO (Maybe [String])
dependencies = shelly . silently . liftM (fmap (map T.unpack)) . dependenciesSh


--Get dependencies by performing a dry-run installation.
--Use errExit false as we will use lastExitCode to see if it failed then wrap result in a maybe
dependenciesSh :: String -> Sh (Maybe [T.Text])
dependenciesSh name = errExit False $ do depText <- run "cabal" ["install", package , "--dry-run"]
                                         exitCode <- lastExitCode
                                         if exitCode /= 0 then return . Just $ processOutput depText
                                         	              else return Nothing
    where package = T.pack name
          --Process the output of the dry run into the dependencies
          --Split into lines, drop the first 2 lines which are boilerplate
          -- drop the last line which is this package
          --On each line only take upto the first space - to ignore the lastest
          -- information given on some lines
          processOutput = map dropLatest . drop 2 . init . T.lines
          dropLatest = head . T.splitOn " "

--Build the current list of packages return the success or failure of the build.
build :: [String] -> IO Bool
build = shelly . silently . errExit False . buildSh -- Use exitErr as we will be checking lastExitCode rather than for an exception.

buildSh :: [String] -> Sh Bool
buildSh = undefined

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