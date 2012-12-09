
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

import Language.Modulo
import Language.Modulo.C
import Language.Modulo.Lisp
import Language.Modulo.Load
import Language.Modulo.Parse
import Language.Modulo.Rename
import Language.Modulo.Util

data ModLang
    = C
    | Lisp
    | JavaScript
    | Haskell
    deriving (Eq, Read, Show)
modLangs = [C, Lisp, JavaScript, Haskell]

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
    | Package { getPackage :: String }
    | Lang { getLang :: ModLang }
    | Path { getPath :: [ModulePath] }
    deriving (Eq, Show)

readModLang :: Maybe String -> ModOpt
readModLang Nothing = Lang C
readModLang (Just s) = Lang $ case s of
    "l"          -> Lisp
    "lisp"       -> Lisp
    "Lisp"       -> Lisp
    "js"         -> JavaScript
    "JS"         -> JavaScript
    "Js"         -> JavaScript
    "JavaScript" -> JavaScript
    "hs"         -> Haskell
    "haskell"    -> Haskell
    "Haskell"    -> Haskell
    _            -> C

readModPath :: Maybe String -> ModOpt
readModPath = Path . maybeToList
-- TODO accept more than one, separate by commas

readPackage :: Maybe String -> ModOpt
readPackage = Package . maybe "user" id


version = "modulo-1.0"
header  = "Usage: modulo [options]\n" ++
          "Usage: modulo [options] files...\n" ++
          "\n" ++
          "Languages:\n  " ++ concatSep "\n  " (map show modLangs) ++ "\n" ++
          "\n" ++
          "Options:"

options = [ 
    (Option ['h'] ["help"]          (NoArg Help)         "Print help and exit"),
    (Option ['v'] ["version"]       (NoArg Version)      "Print version and exit"),
    (Option ['L'] ["language"]      (OptArg readModLang  "LANG") "Output language"),
    (Option ['M'] ["module-path"]   (OptArg readModPath  "PATH") "Module paths"),
    (Option []    ["lisp-package"]  (OptArg readPackage  "STRING") "Lisp package")
  ]                                          
    
main = do
    (opts, args, optErrs) <- getOpt Permute options `fmap` getArgs

    let usage = usageInfo header options
    let printUsage   = putStr (usage ++ "\n") >> exitWith ExitSuccess
    let printVersion = putStr (version ++ "\n") >> exitWith ExitSuccess

    when (Help `elem` opts) printUsage
    when (Version `elem` opts) printVersion  
    runFilter opts


findLang opts = fmap getLang $ find isLang opts
    where                                   
        isLang (Lang _) = True
        isLang _        = False
findPath opts = fmap getPath $ find isPath opts
    where                                   
        isPath (Path _) = True
        isPath _        = False
findPackage opts = fmap getPackage $ find isPackage opts
    where                                   
        isPackage (Package _) = True
        isPackage _           = False
        
-- |
-- Run as a filter from stdin to stdout.
runFilter :: [ModOpt] -> IO ()
runFilter opts = compileFile opts stdin stdout


compileFile :: [ModOpt] -> Handle -> Handle -> IO ()
compileFile opts input output = do
    let lang        = fromMaybe C (findLang opts)
    let paths       = fromMaybe [] (findPath opts)
    let package     = fromMaybe "user" (findPackage opts)
    
    s <- hGetContents input
    let m  = unsafeParse s
    mr <-    unsafeRename paths m
    let c  = printMod lang mr
    hPutStr output c
    
    return ()
    where           
        unsafeRename :: [ModulePath] -> Module -> IO Module
        unsafeRename paths m = do
            deps <- loadDependencies (withStdModulePaths paths) m
            return $ rename deps m
            
        unsafeParse :: String -> Module
        unsafeParse s = case (parse s) of
            Left e -> error $ "Parse error: " ++ show e
            Right m -> m
        
        printMod :: ModLang -> Module -> String
        printMod C    = printModule
        printMod Lisp = printModuleLisp
                                                 