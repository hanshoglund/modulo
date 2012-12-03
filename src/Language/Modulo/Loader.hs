
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
        ModulePath,
        stdModulePaths,
        withStdModulePaths,
        relativePath,
        absolutePaths,
        loadModuleDeps,
        loadRec
  ) where

import Control.Exception

import Language.Modulo
import Language.Modulo.Parser
import Language.Modulo.Util
import qualified Data.List.NonEmpty as NonEmpty

-- We should mimic CPP behaviour or it is a bug

type ModulePath = FilePath

-- |
-- Module paths, in order of preference.
stdModulePaths :: [ModulePath]
stdModulePaths = ["/usr/modules", "/usr/local/modules"]

-- |
-- Append the standard paths to the given paths.
withStdModulePaths :: [ModulePath] -> [ModulePath]
withStdModulePaths = (++ stdModulePaths)

-- |
-- Converts a module name to a relative path.
relativePath :: ModuleName -> ModulePath
relativePath n = concatSep "/" (getModuleNameList n) ++ ".module"

-- |
-- Converts a module name to an absolute path.
absolutePaths :: [ModulePath] -> ModuleName -> [ModulePath]
absolutePaths ps n = map (++ "/" ++ relativePath n) ps


loadModuleDeps :: [ModulePath] -> Module -> IO [Module]
loadModuleDeps ps m = do
    let deps = modImports m
    depsM <- concatMapM (loadRec ps) deps
    return $ m : depsM



-- TODO detect and fail on recursive dependencies
loadRec :: [ModulePath] -> ModuleName -> IO [Module]
loadRec ps n = do
    m <- unsafeLoad ps n
    let deps = modImports m
    depsM <- concatMapM (loadRec ps) deps
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
