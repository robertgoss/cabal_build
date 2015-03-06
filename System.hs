{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module System(packageList,dependencies,build) where

--Import of shelly library - it defaults to using Text instead of string
--  This reduces coercion for literals by default to Text.
import Shelly
import qualified Data.Text as T
default (T.Text)

--File for direct (lowlevel) interface with cabal commands

--Get the list of names of all currently available packages to the cabal system
packageList :: IO [String]
packageList = shelly . silently $ packageListSh

packageListSh :: Sh [String]
packageListSh = undefined

--Given the current package-name find the dependent packages that would need
-- to be installed. This is wrapped in a maybe to detect a failure in resolution.
dependencies :: String -> IO (Maybe [String])
dependencies = shelly . silently . dependenciesSh

dependenciesSh :: String -> Sh (Maybe [String])
dependenciesSh = undefined

--Build the current list of packages return the success or failure of the build.
build :: [String] -> IO Bool
build = shelly . silently . buildSh

buildSh :: [String] -> Sh Bool
buildSh = undefined

-- Internal commands to help manage the system state.

--Clean the cabal local state.
-- Uninstalls all the local packages - by nuking the local package lists and cabal local builds
-- This is inefficient but the easiest way to start off.
cleanCabalSystem :: IO ()
cleanCabalSystem = shelly . silently $ cleanCabalSystemSh

cleanCabalSystemSh :: Sh ()
cleanCabalSystemSh = undefined