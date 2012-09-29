
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
-- License     : BSD-style
-- Maintainer  : hans@hanshoglunds.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Language.Modulo.Parser (
        parse,
        parseTest
  ) where

import Control.Monad

import Text.Parsec hiding (parse, parseTest)
import Text.Parsec.Token
import Text.Parsec.String

import Language.Modulo

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty
import Data.Functor.Identity-- FIXME

parse :: String -> Either ParseError Module
parse str = runParser parseModule () "" str

parseTest :: FilePath -> IO Module
parseTest path = do 
    str <- readFile path
    case (runParser parseModule () path str) of
        Left e -> error . show $ e
        Right m -> return m


-- Util

-- Parse repeated element with unique prefix.
manyLinear :: (Show a, Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
manyLinear x = x `manyTill` (notFollowedBy x)

-- Parser

parseModule :: Parser Module
parseModule = do
    optional lspace
    string "module"        
    optional lspace
    name <- parseModuleName
    optional lspace
    optional lspace
    
    string "{"
    optional lspace
    
    mimp <- manyLinear parseImport
    
    -- decls
    string "}"
        
    return $ Module name mimp []

parseModuleName :: Parser ModuleName
parseModuleName = do
    (x:xs) <- lname `sepBy1` (string ".")
    return . ModuleName $ x :| xs

parseImport :: Parser ModuleName
parseImport = do
    string "import"
    optional lspace
    x <- parseModuleName
    optional lspace
    string ";"
    optional lspace
    return x

parseDeclaration :: Parser Declaration
parseDeclaration = undefined


parseTypeDec :: Parser Declaration
parseTypeDec = undefined

parseFunDec :: Parser Declaration
parseFunDec = undefined

parseConstDec :: Parser Declaration
parseConstDec = undefined

parseGlobalDec :: Parser Declaration
parseGlobalDec = undefined


parseType :: Parser Type     
parseType = undefined

parseFunctionType :: Parser FunctionType
parseFunctionType = undefined

parsePrimType :: Parser PrimType      
parsePrimType = undefined



lexer :: TokenParser ()
lexer = makeTokenParser $ 
    LanguageDef { commentStart    =  "/*",
                  commentEnd      =  "*/",
                  commentLine     =  "//",
                  nestedComments  =  True,
                  identStart      =  (letter <|> char '_'),
                  identLetter     =  (letter <|> char '_'),
                  opStart         =  mzero,
                  opLetter        =  mzero,
                  reservedNames   =  mzero, -- TODO module import struct union data enum type
                  reservedOpNames =  mzero,
                  caseSensitive   =  True }  

lname  = identifier lexer
lspace = whiteSpace lexer    
