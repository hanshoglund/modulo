
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
-- Parser for the module language, as described in "Language.Modulo".
--
-------------------------------------------------------------------------------------

module Language.Modulo.Parse (
        parse,
        unsafeParseFile
  ) where

import Control.Monad
import Control.Applicative hiding ((<|>), optional, many)

import Text.Parsec hiding (parse)
import Text.Parsec.Token
import Text.Parsec.String

import Language.Modulo
import Language.Modulo.Util

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty

-- |
-- Parse a module description from a string, returning an error if unsuccessful and
-- the resulting module otherwise.
--
parse :: String -> Either ParseError Module
parse str = runParser parseModule () "" str

-- |
-- Parse a module description from the given file, or fail if unsuccessful.
--
-- This unsafe function should not be used in production code.
--
unsafeParseFile :: FilePath -> IO Module
unsafeParseFile path = do
    str <- readFile path
    case (runParser parseModule () path str) of
        Left e -> error . show $ e
        Right m -> return m

-- unsafeParseFile2 path = do
--     m <- unsafeParseFile path
--     putStrLn . breakList 80 "\n" . show $ m


-------------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------------

parseModule :: Parser Module
parseModule = do
    optional lspace
    reserved lexer "module"
    name <- parseModuleName
    char '{'
    optional lspace
    imps <- many parseImport
    decls <- many parseDecl
    char '}'
    optional lspace
    return $ Module name imps decls

parseModuleName :: Parser ModuleName
parseModuleName = do
    (x:xs) <- identifier lexer `sepBy1` (string ".")
    return . ModuleName $ x :| xs

parseImport :: Parser ModuleName
parseImport = do
    reserved lexer "import"
    x <- parseModuleName
    semi lexer
    return x

parseNameType :: Parser (Name, Type)
parseNameType = do
    name <- lname
    char ':'
    optional lspace
    typ <- parseType
    return $ (Name name, typ)

parseDecl :: Parser Decl
parseDecl = mzero
    <|> parseTypeDec
    <|> parseTagDec
    <|> parseFunDec
    -- <|> parseConstDec
    -- <|> parseGlobalDec

parseTypeDec :: Parser Decl
parseTypeDec = do
    reserved lexer "type"
    name <- lname
    char '='
    optional lspace
    typ <- parseType
    semi lexer
    return $ TypeDecl (Name name) typ

parseTagDec :: Parser Decl
parseTagDec = do
    reserved lexer "tagname"
    typ <- parseType
    semi lexer
    return $ TagDecl typ

-- TODO handle non-function types
parseFunDec :: Parser Decl
parseFunDec = do
    (name, FunType typ) <- parseNameType
    semi lexer
    return $ FunctionDecl name typ

parseConstDec :: Parser Decl
parseConstDec = error "Can not parse constants yet"

parseGlobalDec :: Parser Decl
parseGlobalDec = error "Can not parse globals yet"


parseType :: Parser Type
parseType = do
    typ <- parseTypeStart
    -- Check for postfix *
    n <- occs (char '*')
    return $ times n (RefType . Pointer) typ
    where
        times 0 f = id
        times n f = f . times (n-1) f

parseTypeStart :: Parser Type
parseTypeStart = mzero
    <|> parseArrayType
    <|> parseFunType
    <|> parseEnumType
    <|> parseUnionType
    <|> parseStructType
    <|> parseBitfieldType
    <|> parsePrimType
    <|> parseAliasType


parseArrayType :: Parser Type
parseArrayType = do
    char '['
    optional lspace
    typ <- parseType
    optional lspace
    char 'x'
    optional lspace
    n <- lnat
    char ']'
    optional lspace
    return $ RefType $ Array typ (fromInteger n)

parseFunType :: Parser Type
parseFunType = do
    char '('
    optional lspace
    args <- parseType `sepBy` (spaceAround $ char ',')
    optional lspace
    char ')'
    optional lspace
    string "->"
    optional lspace
    res <- parseType
    return $ FunType $ Function args res


parseEnumType :: Parser Type
parseEnumType = do
    reserved lexer "enum"
    char '{'
    optional lspace
    (n:ns) <- lname `sepBy` (spaceAround $ char ',')
    optional lspace
    char '}'
    optional lspace
    return $ CompType $ Enum (fmap Name $ n :| ns)

parseUnionType :: Parser Type
parseUnionType = do
    reserved lexer "union"
    char '{'
    optional lspace
    (n:ns) <- parseNameType `sepBy` (spaceAround $ char ',')
    optional lspace
    char '}'
    return $ CompType $ Union (n :| ns)

parseStructType :: Parser Type
parseStructType = do
    reserved lexer "struct"
    char '{'
    optional lspace
    (n:ns) <- parseNameType `sepBy1` (spaceAround $ char ',')
    optional lspace
    char '}'
    return $ CompType $ Struct (n :| ns)

parseBitfieldType :: Parser Type
parseBitfieldType = do
    reserved lexer "bitfield"
    error "Can not parse bitfields yet"

parseAliasType :: Parser Type
parseAliasType = do
    name <- lname
    return $ AliasType (Name name)

parsePrimType :: Parser Type
parsePrimType = mzero
    <|> "Int8"          ==> Int8
    <|> "Int16"         ==> Int16
    <|> "Int32"         ==> Int32
    <|> "Int64"         ==> Int64
    <|> "UInt8"         ==> UInt8
    <|> "UInt16"        ==> UInt16
    <|> "UInt32"        ==> UInt32
    <|> "UInt64"        ==> UInt64

    <|> "Bool"          ==> Bool
    <|> "Void"          ==> Void
    <|> "Size"          ==> Size
    <|> "Ptrdiff"       ==> Ptrdiff
    <|> "Intptr"        ==> Intptr
    <|> "UIntptr"       ==> UIntptr

    <|> "Char"          ==> Char
    <|> "Short"         ==> Short
    <|> "Int"           ==> Int
    <|> "Long"          ==> Long
    <|> "LongLong"      ==> LongLong

    <|> "UChar"         ==> UChar
    <|> "UShort"        ==> UShort
    <|> "UInt"          ==> UInt
    <|> "ULong"         ==> ULong
    <|> "ULongLong"     ==> ULongLong

    <|> "Float"         ==> Float
    <|> "Double"        ==> Double
    <|> "LongDouble"    ==> LongDouble
    where
        (==>) s t = lres s >> return (PrimType t)


-- Extra combinators, not exported
occs p        = length <$> many p
spaceBefore p = optional lspace >> p
spaceAfter p  = p >> optional lspace
spaceAround p = spaceBefore (spaceAfter p)

-------------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------------

lexer :: TokenParser ()
lexer = makeTokenParser $
    LanguageDef { 
        commentStart    =  "/*",
        commentEnd      =  "*/",
        commentLine     =  "//",
        nestedComments  =  True,
        identStart      =  (letter <|> char '_'),
        identLetter     =  (letter <|> char '_'),
        opStart         =  mzero,
        opLetter        =  mzero,
        reservedNames   =  reservedNames,
        reservedOpNames =  mzero,
        caseSensitive   =  True }
    where
        reservedNames = [
            "module", "import", "type", "tagname", "enum", "union", "struct", "bitfield",
            "Int", "Void", "Size", "Ptrdiff", "Intptr", "UIntptr",
            "Char", "Short", "Int", "Long", "LongLong",
            "UChar", "UShort", "UInt", "ULong", "ULongLong",
            "Float", "Double", "LongDouble",
            "Int8", "Int16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64" ]

-- Convenient synonyms, not exported
lnat   = natural lexer
lname  = identifier lexer
lres   = reserved lexer
lspace = whiteSpace lexer

