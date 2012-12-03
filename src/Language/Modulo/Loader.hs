
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

module Language.Modulo.Loader (
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
import Language.Modulo.Parser
import Language.Modulo.Util
import qualified Data.List.NonEmpty as NonEmpty

-- We should mimic CPP behaviour or it is a bug

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


-- |
-- Load the dependencies of the given module.
--
-- This function will traverse the loaded module recursively and block if a recursive dependency
-- is encountered.
-- 
loadDependencies :: [ModulePath] -> Module -> IO [Module]
loadDependencies ps m = do
    let deps = modImports m
    depsM <- concatMapM (loadModule ps) deps
    return $ m : depsM



-- TODO detect and fail on recursive dependencies

-- |
-- Load a module of the given name.
-- 
loadModule :: [ModulePath] -> ModuleName -> IO [Module]
loadModule ps n = do
    m <- unsafeLoad ps n
    let deps = modImports m
    depsM <- concatMapM (loadModule ps) deps
    return $ m : depsM
    
    where
        unsafeLoad :: [ModulePath] -> ModuleName -> IO Module
        unsafeLoad ps n = do
            s <- readFilePref $ absolutePaths ps n
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
readFilePref :: [FilePath] -> IO String
readFilePref []     = error "readFilePref: Empty path list"
readFilePref (f:fs) = do
    r <- try $ readFile f :: IO (Either IOException String)
    case r of
        Left e  -> if (null fs) then (error $ show e) else readFilePref fs
        Right s -> return s

getModuleNameList :: ModuleName -> [String]
getModuleNameList = NonEmpty.toList . getModuleName

concatMapM :: (Monad f, Functor f) => (a -> f [b]) -> [a] -> f [b]
concatMapM f = fmap concat . mapM f
