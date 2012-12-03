
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (when)
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Pretty

import Language.Modulo
import Language.Modulo.C
import Language.Modulo.Lisp
import Language.Modulo.Parser
import Language.Modulo.Loader

data ModLang
    = C
    | Lisp
    | Haskell
    deriving (Eq, Read, Show)

data ModCStyle
    = CStyleStd
    | CStyleCairo
    | CStyleGtk
    | CStyleApple
    | CStyleHaskell
    deriving (Eq, Show)
    
data ModOpt
    = Help    
    | Version
    | Lang { getLang :: ModLang }
    | Path { getPath :: [ModulePath] }
    deriving (Eq, Show)

readModLang :: Maybe String -> ModOpt
readModLang Nothing = Lang C
readModLang (Just s) = Lang $ case s of
    "l"         -> Lisp
    "lisp"      -> Lisp
    "Lisp"      -> Lisp
    "hs"        -> Haskell
    "haskell"   -> Haskell
    "Haskell"   -> Haskell
    _           -> C
readModPath :: Maybe String -> ModOpt
readModPath s = Path $ maybeToList s
-- TODO accept more than one, separate by commas

version = "modulo-0.5"
header  = "Usage: modulo [options] files...\n" ++
          "Options:"

options = [ 
    (Option ['h'] ["help"]          (NoArg Help)         "Print help and exit"),
    (Option ['v'] ["version"]       (NoArg Version)      "Print version and exit"),
    (Option ['L'] ["language"]      (OptArg readModLang  "LANG") "Output language"),
    (Option ['M'] ["module-path"]   (OptArg readModPath  "PATH") "Module paths")
  ]                                          
    
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo header options
    let printUsage   = putStr (usage ++ "\n") >> exitWith ExitSuccess
    let printVersion = putStr (version ++ "\n") >> exitWith ExitSuccess

    when (Help `elem` opts) printUsage
    when (Version `elem` opts) printVersion  
    runFilter opts


-- |
-- Run as a filter from stdin to stdout.
runFilter :: [ModOpt] -> IO ()
runFilter opts = compileFile opts stdin stdout

findLang opts = fmap getLang $ find isLang opts
    where                                   
        isLang (Lang _) = True
        isLang _        = False
findPath opts = fmap getPath $ find isPath opts
    where                                   
        isPath (Path _) = True
        isPath _        = False
        

compileFile :: [ModOpt] -> Handle -> Handle -> IO ()
compileFile opts input output = do
    let lang  = fromMaybe C (findLang opts)
    let paths = fromMaybe [] (findPath opts)
    
    s <- hGetContents input
    let m = unsafeParse s
    let c = printMod lang m
    hPutStr output c
    
    return ()
    where               
        unsafeParse :: String -> Module
        unsafeParse s = case (parse s) of
            Left e -> error $ "Parse error: " ++ show e
            Right m -> m
        
        printMod :: ModLang -> Module -> String
        printMod C    = printModule
        printMod Lisp = printModuleLisp
                                                 