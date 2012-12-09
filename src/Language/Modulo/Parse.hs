
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
        parseName,
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
-- Parse a module description, returning an error if unsuccessful.
--
parse :: String -> Either ParseError Module
parse = runParser modParser () ""


-- |
-- Parse a qualified name, returning an error if unsuccessful.
--
parseName :: String -> Either ParseError Name
parseName = runParser nameParser () ""

-- |
-- Parse a module description from the given file, or fail if unsuccessful.
--
-- This unsafe function should not be used in production code.
--
unsafeParseFile :: FilePath -> IO Module
unsafeParseFile path = do
    str <- readFile path
    case (runParser modParser () path str) of
        Left e -> error . show $ e
        Right m -> return m


-------------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------------

modParser :: Parser Module
modParser = do
    optional lspace
    reserved lexer "module"
    name <- modNameParser
    llex $ char '{'
    imps <- many impParser
    decls <- many declParser
    llex $ char '}'
    return $ Module name imps decls

modNameParser :: Parser ModuleName
modNameParser = do
    (x:xs) <- identifier lexer `sepBy1` (string ".")
    return . ModuleName $ x :| xs

impParser :: Parser ModuleName
impParser = do
    reserved lexer "import"
    x <- modNameParser
    semi lexer
    return x

declParser :: Parser Decl
declParser = mzero
    <|> typeDeclParser
    <|> tagDeclParser
    <|> funDeclParser
    -- <|> constDeclParser
    -- <|> globalDeclParser

typeDeclParser :: Parser Decl
typeDeclParser = do
    reserved lexer "type"
    name <- unameParser
    llex $ char '='
    typ <- typeOpaqueParser
    semi lexer
    return $ TypeDecl name typ

tagDeclParser :: Parser Decl
tagDeclParser = do
    reserved lexer "tagname"
    typ <- typeParser
    semi lexer
    return $ TagDecl typ


-- TODO rewrite in declarative style
funDeclParser :: Parser Decl
funDeclParser = do
    (name, typ) <- unameTypeParser
    semi lexer
    case typ of
        (FunType func) -> return $ FunctionDecl name func 
        _              -> unexpected "Expected function type"


constDeclParser :: Parser Decl
constDeclParser = notSupported "Constant parsing"

globalDeclParser :: Parser Decl
globalDeclParser = notSupported "Global parsing"

-------------------------------------------------------------------------------------

typeOpaqueParser :: Parser (Maybe Type)
typeOpaqueParser = (opaqueParser >> return Nothing) <|> fmap Just typeParser

opaqueParser :: Parser ()
opaqueParser = reserved lexer "opaque" >> return ()

typeParser :: Parser Type
typeParser = do
    typ <- typeStartParser
    mods <- typeEndParser
    return $ mods typ

typeEndParser :: Parser (Type -> Type)
typeEndParser = foldr (flip (.)) id <$> quals
    where
        quals = many (ptr <|> func)
        ptr  = (llex $ char '*') >> return (RefType . Pointer) 
        func = do
            llex $ string "->"
            typ <- typeParser
            return $ funRes typ
            
funRes :: Type -> Type -> Type
funRes r a = FunType $ Function [a] r
            

typeStartParser :: Parser Type
typeStartParser = mzero
    <|> parenTypeParser
    <|> arrayTypeParser
    <|> enumTypeParser
    <|> unionTypeParser
    <|> structTypeParser
    <|> bitfieldTypeParser
    <|> primTypeParser
    <|> aliasTypeParser

parenTypeParser :: Parser Type
parenTypeParser = do
    llex $ char '('
    typ <- typeParser
    llex $ char ')'
    return typ

-- funTypeParser :: Parser Type
-- funTypeParser = do
--     char '('
--     optional lspace
--     args <- typeParser `sepBy` (spaceAround $ char ',')
--     optional lspace
--     char ')'
--     optional lspace
--     string "->"
--     optional lspace
--     res <- typeParser
--     return $ FunType $ Function args res

arrayTypeParser :: Parser Type
arrayTypeParser = do
    llex $ char '['
    typ <- typeParser
    llex $ char 'x'
    n <- lnat
    llex $ char ']'
    return $ RefType $ Array typ (fromInteger n)

enumTypeParser :: Parser Type
enumTypeParser = do
    reserved lexer "enum"
    llex $ char '{'
    (n:ns) <- unameParser `sepBy` (llex $ char ',')
    llex $ char '}'
    return $ CompType $ Enum (n :| ns)

structTypeParser :: Parser Type
structTypeParser = do
    reserved lexer "struct"
    llex $ char '{'
    (n:ns) <- unameTypeParser `sepBy1` (llex $ char ',')
    llex $ char '}'
    return $ CompType $ Struct (n :| ns)

unionTypeParser :: Parser Type
unionTypeParser = do
    reserved lexer "union"
    llex $ char '{'
    (n:ns) <- unameTypeParser `sepBy1` (llex $ char ',')
    llex $ char '}'
    return $ CompType $ Union (n :| ns)

bitfieldTypeParser :: Parser Type
bitfieldTypeParser = do
    reserved lexer "bitfield"
    notSupported "Bitfield parsing"

primTypeParser :: Parser Type
primTypeParser = mzero
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
        s ==> t = lres s >> return (PrimType t)

aliasTypeParser :: Parser Type
aliasTypeParser = do
    name <- nameParser
    return $ AliasType name

-------------------------------------------------------------------------------------

nameParser :: Parser Name
nameParser = do
    r <- identifier lexer `sepBy1` (string ".")
    return $ case r of
        [x]    -> Name x
        (x:xs) -> QName (ModuleName $ x :| init xs) (last xs)

unameParser :: Parser Name
unameParser = Name <$> lname

unameTypeParser :: Parser (Name, Type)
unameTypeParser = do
    name <- unameParser
    llex $ char ':'
    typ <- typeParser
    return $ (name, typ)
                             

-- Extra combinators, not exported
occs p        = length <$> many p
follow p q    = do
    a <- p
    b <- q
    return (a, b)
-- spaceBefore p = optional lspace >> p
-- spaceAfter p  = p >> optional lspace
-- spaceAround p = spaceBefore (spaceAfter p)

-------------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------------

lexer :: TokenParser ()
lexer = makeTokenParser $ LanguageDef { 
    commentStart    =  "/*",
    commentEnd      =  "*/",
    commentLine     =  "//",
    nestedComments  =  True,
    identStart      =  (letter <|> char '_'),
    identLetter     =  (alphaNum <|> char '_'),
    opStart         =  mzero,
    opLetter        =  mzero,
    reservedNames   =  reservedNames,
    reservedOpNames =  mzero,
    caseSensitive   =  True 
    }
    where
        reservedNames = [
            "module", "import", "type", "tagname", "opaque", "enum", "union", "struct", "bitfield",
            "Int", "Void", "Size", "Ptrdiff", "Intptr", "UIntptr",
            "Char", "Short", "Int", "Long", "LongLong",
            "UChar", "UShort", "UInt", "ULong", "ULongLong",
            "Float", "Double", "LongDouble",
            "Int8", "Int16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64" ]

-- Convenient synonyms, not exported
llex   = lexeme lexer
lnat   = natural lexer
lname  = identifier lexer
lres   = reserved lexer
lspace = whiteSpace lexer


notSupported x = error $ "Not supported yet: " ++ x

