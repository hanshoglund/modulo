
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (when)
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Pretty

import Language.Modulo
import Language.Modulo.C
import Language.Modulo.Lisp
import Language.Modulo.Parser

data ModOutputLang
    = C
    | Lisp
    | Haskell
    deriving (Eq, Show)

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
    | Language
    deriving (Eq, Show)

version = "modulo-0.5"
header  = "Usage: modulo [options] files...\n" ++
          "Options:"

options = [ 
    (Option ['h'] ["help"]          (NoArg Help)        "Print help and exit"),
    (Option ['v'] ["version"]       (NoArg Version)     "Print version and exit")
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

compileFile :: [ModOpt] -> Handle -> Handle -> IO ()
compileFile opts input output = do
    s <- hGetContents input
    let m = parseSafe s
    let c = printModule m
    hPutStr output c

    -- let coreToCore = singleAbs . singleApp
    -- let hs = parse s
    -- let b  = fromHaskell hs
    -- let b' = transform coreToCore b
    -- let ms = toManuScript b'
    -- 
    -- when (IncludeCore `elem` opts || JustCore `elem` opts) $ do
    --     hPutStr output $ show (pretty b')
    --     hPutStr output "\n"
    --     hPutStr output "\n"
    -- 
    -- when (not $ JustCore `elem` opts) $ do
    --     hPutStr output $ show (pretty ms)
    --     hPutStr output "\n"
    --     hPutStr output "\n"
    
    return ()
    where               
        parseSafe :: String -> Module
        parseSafe s = case (parse s) of
            Left e -> error $ "Parse error: " ++ show e
            Right m -> m
                                                 