
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
  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String

import Language.Modulo

mod :: Parser Module
mod = undefined
nam :: Parser ModuleName
nam = undefined
imp :: Parser [ModuleName]
imp = undefined
decs :: Parser Declaration
decs = undefined

typeDec :: Parser Declaration
typeDec = undefined
funDec :: Parser Declaration
funDec = undefined
constDec :: Parser Declaration
constDec = undefined
globalDec :: Parser Declaration
globalDec = undefined

typ :: Parser Type     
typ = undefined
funTyp :: Parser FunctionType
funTyp = undefined
prim :: Parser PrimType      
prim = undefined

lexer :: TokenParser ()
lexer = makeTokenParser $ 
    LanguageDef { commentStart    =  "/*",
                  commentEnd      =  "*/",
                  commentLine     =  "//",
                  nestedComments  =  True,
                  identStart      =  (letter <|> char '_'),
                  identLetter     =  (alphaNum <|> char '_'),
                  opStart         =  mzero,
                  opLetter        =  mzero,
                  reservedNames   =  mzero, -- TODO module import struct union data enum type
                  reservedOpNames =  mzero,
                  caseSensitive   =  True }  

lname  = identifier lexer
lspace = whiteSpace lexer    
