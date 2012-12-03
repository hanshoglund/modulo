
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

module Language.Modulo.Loader -- (
  --)
where

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
withStdModulePaths ps = ps ++ stdModulePaths

relativePath :: ModuleName -> ModulePath
relativePath n = concatSep "/" (getModuleNameList n) ++ ".module"

getModuleNamePaths :: ModuleName -> [ModulePath] -> [FilePath]
getModuleNamePaths n ps = map (++ "/" ++ relativePath n) ps





loadRec :: ModuleName -> [ModulePath] -> IO [Module]
loadRec n ps = do
    s <- readFilePref $ getModuleNamePaths n ps
    let m = unsafeParse s
    let deps = modImports m
    depsM <- fmap concat $ mapM (\d -> loadRec d ps) deps
    return depsM
    
    where
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

getModuleNameList = NonEmpty.toList . getModuleName
