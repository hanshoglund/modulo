
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
-------------------------------------------------------------------------------------

module Language.Modulo.Parser (
        parse,
        parseTest
  ) where

import Control.Monad
import Control.Applicative hiding ((<|>), optional, many)

import Text.Parsec hiding (parse, parseTest)
import Text.Parsec.Token
import Text.Parsec.String

import Language.Modulo
import Language.Modulo.Util

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty

parse :: String -> Either ParseError Module
parse str = runParser parseModule () "" str

parseTest :: FilePath -> IO Module
parseTest path = do
    str <- readFile path
    case (runParser parseModule () path str) of
        Left e -> error . show $ e
        Right m -> return m

parseTest2 path = do 
    m <- parseTest path
    putStrLn . breakList 80 "\n" . show $ m

-- Util

-- Parse repeated element with unique prefix.
-- manyLinear :: (Show a, Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
-- manyLinear p = p `manyTill` (notFollowedBy p)

occs p = length <$> many p


spaceBefore p = optional lspace >> p
spaceAfter p  = p >> optional lspace
spaceAround p = spaceBefore (spaceAfter p)


-- Parser

parseModule :: Parser Module
parseModule = do
    optional lspace
    reserved lexer "module"
    name <- parseModuleName
    char '{'
    optional lspace
    imps <- many parseImport
    decls <- many parseDeclaration
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

parseDeclaration :: Parser Declaration
parseDeclaration = mzero
    <|> parseTypeDec
    -- <|> parseTagDec
    -- <|> parseFunDec
    -- <|> parseConstDec
    -- <|> parseGlobalDec

parseTypeDec :: Parser Declaration
parseTypeDec = do
    reserved lexer "type"
    name <- lname
    char '='
    optional lspace
    typ <- parseType
    semi lexer
    return $ TypeDecl name typ
 
parseTagDec :: Parser Declaration
parseTagDec = do
    reserved lexer "tagname"
    typ <- parseType
    semi lexer
    return $ TagDecl typ
    
parseConstDec :: Parser Declaration
parseConstDec = error "Can not parse constants yet"

parseGlobalDec :: Parser Declaration
parseGlobalDec = error "Can not parse globals yet"

parseFunDec :: Parser Declaration
parseFunDec = error "Can not parse function declarations yet"


parseType :: Parser Type
parseType = do
    typ <- parseTypeStart
    n <- occs (char '*')
    return $ times n (PointerType . Pointer) typ
    where
        times 0 f = id
        times n f = f . times (n-1) f
        
parseTypeStart :: Parser Type
parseTypeStart = mzero
    <|> parseArrayType
    <|> parseFunctionType
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
    return $ PointerType $ Array typ (fromInteger n)

parseFunctionType :: Parser Type
parseFunctionType = do
    char '('
    optional lspace
    args <- parseType `sepBy` (spaceAround $ char ',')
    optional lspace
    char ')'
    optional lspace
    string "->"
    optional lspace
    res <- parseType
    return $ FunctionType $ Function args res
    

parseEnumType :: Parser Type
parseEnumType = do
    reserved lexer "enum"
    char '{'
    optional lspace
    (n:ns) <- lname `sepBy` (spaceAround $ char ',')
    optional lspace
    char '}'
    optional lspace
    return $ CompoundType $ Enum (n :| ns)

parseUnionType :: Parser Type
parseUnionType = do
    reserved lexer "union"
    char '{'
    optional lspace
    (n:ns) <- parseNameType `sepBy` (spaceAround $ char ',')
    optional lspace
    char '}'
    return $ CompoundType $ Union (n :| ns)

parseStructType :: Parser Type
parseStructType = do
    reserved lexer "struct"
    char '{'
    optional lspace
    (n:ns) <- parseNameType `sepBy` (spaceAround $ char ',')
    optional lspace
    char '}'
    return $ CompoundType $ Struct (n :| ns)

parseBitfieldType :: Parser Type
parseBitfieldType = do
    reserved lexer "bitfield"
    error "Can not parse bitfields yet"


-- will choice work here? need backtracking?
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

    <|> "Int"           ==> Int
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
        
parseAliasType :: Parser Type
parseAliasType = error "Can not parse type aliases yet"
        
        
parseNameType :: Parser (Name, Type)
parseNameType = do
    name <- lname
    optional lspace
    char ':'
    optional lspace
    typ <- parseType
    return $ (name, typ)




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
                  reservedNames   =  reservedNames,
                  reservedOpNames =  mzero,
                  caseSensitive   =  True }        

    where
        reservedNames = [
            "module",
            "import",
            "type",
            "tagname",
            "enum",
            "union",
            "struct",
            "bitfield",

            "Int",
            "Void", 
            "Size", 
            "Ptrdiff", 
            "Intptr", 
            "UIntptr", 
            
            "Char", 
            "Short", 
            "Int", 
            "Long", 
            "LongLong", 
            
            "UChar", 
            "UShort", 
            "UInt", 
            "ULong", 
            "ULongLong", 
            
            "Float", 
            "Double", 
            "LongDouble", 
            
            "Int8", 
            "Int16", 
            "Int32", 
            "Int64", 
            "UInt8", 
            "UInt16", 
            "UInt32", 
            "UInt64" ] 
                  

lnat   = natural lexer
lname  = identifier lexer
lres   = reserved lexer
lspace = whiteSpace lexer
