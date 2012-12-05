
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
-- License     : BSD-style
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--
--
-------------------------------------------------------------------------------------

module Language.Modulo.Load (
        -- ** Paths
        ModulePath,
        relativePath,
        absolutePaths,

        -- *** Standard paths
        stdModulePaths,
        withStdModulePaths,

        -- ** Loading
        loadModule,
        loadDependencies,
  ) where

import Control.Exception

import Language.Modulo
import Language.Modulo.Parse
import Language.Modulo.Util

import qualified Data.List.NonEmpty as NonEmpty

-- We should mimic CPP behaviour or it is a bug
-- We use usr/modules in place of usr/include etc.

-- |
-- Path where modules are stored.
--
type ModulePath = FilePath

-- |
-- Module paths, in order of preference.
stdModulePaths :: [ModulePath]
stdModulePaths = ["/usr/modules", "/usr/local/modules"]

-- |
-- Append the standard paths to the given paths.
--
-- That is, the given paths take precedence over the standards.
--
withStdModulePaths :: [ModulePath] -> [ModulePath]
withStdModulePaths = (++ stdModulePaths)

-- |
-- Converts a module name to a relative path.
--
relativePath :: ModuleName -> FilePath
relativePath n = concatSep "/" (getModuleNameList n) ++ ".module"

-- |
-- Converts a module name to a list of absolute paths, in order of preference.
--
absolutePaths :: [ModulePath] -> ModuleName -> [FilePath]
absolutePaths ps n = map (++ "/" ++ relativePath n) ps


-- TODO detect and fail on recursive dependencies
-- TODO generally more safe version of loadModule and loadDepencencies

-- |
-- Load the dependencies of the given module.
--
-- Dependencies of the loaded modules are loaded transitively. 
-- This function blocks if a recursive dependency is encountered.
-- 
loadDependencies :: [ModulePath] -> Module -> IO [Module]
loadDependencies ps m = do
    let depNames = modImports m
    deps <- concatMapM (loadModule ps) depNames
    return $ m : deps


-- |
-- Load a module of the given name.
-- 
-- Dependencies of the loaded modules are loaded transitively. 
-- This function blocks if a recursive dependency is encountered.
-- 
loadModule :: [ModulePath] -> ModuleName -> IO [Module]
loadModule ps n = do
    m <- unsafeLoad ps n
    let depNames = modImports m
    deps <- concatMapM (loadModule ps) depNames
    return $ m : deps
    
    where
        unsafeLoad :: [ModulePath] -> ModuleName -> IO Module
        unsafeLoad ps n = do
            s <- unsafeReadAny $ absolutePaths ps n
            let m = unsafeParse s
            if (modName m /= n)
                then (error $ "File name does not match module name: \n"
                    ++ "    Saw: `" ++ show (modName m) ++ "'\n"
                    ++ "    Expected: `" ++ show n ++ "'\n")
                else (return m)
        
        unsafeParse :: String -> Module
        unsafeParse s = case (parse s) of
            Left e -> error $ "Parse error: " ++ show e
            Right m -> m



-- | 
-- Attempt to read all given files, returning the contents of the first successful 
-- read, or failing if none could be found.
unsafeReadAny :: [FilePath] -> IO String
unsafeReadAny []     = error "unsafeReadAny: Empty path list"
unsafeReadAny (f:fs) = do
    r <- try $ readFile f :: IO (Either IOException String)
    case r of
        Left e  -> if (null fs) then (error $ show e) else unsafeReadAny fs
        Right s -> return s

