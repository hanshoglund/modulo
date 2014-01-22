
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies, OverloadedStrings,
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
        parsePrimType,
        parsePrimTypeMaybe,
        unsafeParseFile
  ) where

import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Default
import Data.Maybe
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
-- Parse a primitive type, returning an error if unsuccessful.
--
parsePrimType :: String -> Either ParseError PrimType
parsePrimType = fmap unPrimType . runParser primTypeParser () ""
    where
        unPrimType (PrimType x) = x

-- |
-- Parse a primitive type, returning an error if unsuccessful.
--
parsePrimTypeMaybe :: String -> Maybe PrimType
parsePrimTypeMaybe = eitherToMaybe . parsePrimType

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

    doc <- fmap (Doc . fromMaybe "") $ optionMaybe docComment
    optional lspace
    
    reserved lexer "module"
    name <- modNameParser
    llex $ char '{'
    imps <- many impParser
    docDecls <- many docDeclParser
    llex $ char '}'
    
    return $ Module name def doc imps docDecls

modNameParser :: Parser ModuleName
modNameParser = do
    (x:xs) <- identifier lexer `sepBy1` (string ".")
    return . ModuleName $ x :| xs

impParser :: Parser (ModuleName, Maybe String)
impParser = do
    reserved lexer "import"
    conv <- optionMaybe lstr
    name <- modNameParser
    semi lexer
    return (name, conv)

docDeclParser :: Parser (Doc, Decl)
docDeclParser = do
    doc <- fmap (Doc . fromMaybe "") $ optionMaybe docComment
    optional lspace
    decl <- declParser
    return $ (doc, decl)

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

-- To parse a type, we first parse all types whose syntax does not involve
-- post-qualifiers (i.e. pointer and function), then parse the modifiers to
-- generate a qualifier, and apply it to the prefix. I.e. in (a -> b) we first
-- parse a, then (-> b), and modify a by applying (-> b).
typeParser :: Parser Type
typeParser = do
    typs <- typeStartParser
    mods <- typeEndParser
    return $ mods typs

typeStartParser :: Parser [(Maybe Name,Type)]
typeStartParser = mzero
    <|> parenTypeParser
    <|> (single.returnPair) <$> arrayTypeParser
    <|> (single.returnPair) <$> enumTypeParser
    <|> (single.returnPair) <$> unionTypeParser
    <|> (single.returnPair) <$> structTypeParser
    <|> (single.returnPair) <$> bitfieldTypeParser
    <|> (single.returnPair) <$> primTypeParser
    <|> (single.returnPair) <$> aliasTypeParser
returnPair x = (Nothing,x)               

typeEndParser :: Parser ([(Maybe Name, Type)] -> Type)
typeEndParser = foldr comp noTypeEnd <$> many (ptr <|> func)
    where
        ptr = do
            llex $ char '*'
            return mkPtr
        func = do
            llex $ string "->"
            typ <- typeParser
            return $ mkFun typ

        noTypeEnd :: [(Maybe Name, Type)] -> Type
        noTypeEnd [(_,x)] = x
        noTypeEnd _       = error "Unexpected argument head"
  
        mkPtr :: [(Maybe Name, Type)] -> Type
        mkPtr [(_,x)] = RefType . Pointer $ x
        mkPtr _       = error "Unexpected argument head"

        mkFun :: Type -> [(Maybe Name, Type)] -> Type
        mkFun r as = FunType $ Function as r        

        -- TODO is this (=>=) in Control.Comonad ?
        comp :: ([(Maybe x,a)] -> b) -> ([(Maybe x,b)] -> c) -> [(Maybe x,a)] -> c
        comp g f = f . single . returnPair . g

-- This is not really a type, but the product used to express the argument to an uncurried function.
-- However, if it is just a one-tuple without a name, it will be treated as an ordinary type.
-- That is (A) -> B is the same as A -> B.

-- TODO support named arguments
parenTypeParser :: Parser [(Maybe Name, Type)]
parenTypeParser = do
    llex $ char '('
    types <-  maybeNameTypeParser `sepBy` (llex $ char ',')
    llex $ char ')'
    return $ types

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


docComment :: Parser String
docComment  = do
    string "/**"
    manyTill anyChar (try (string "*/"))

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

maybeNameTypeParser :: Parser (Maybe Name, Type)
maybeNameTypeParser = try (fmap (first Just) unameTypeParser) <|> fmap returnPair typeParser

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
    commentStart    =  "/*!",
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
lstr   = stringLiteral lexer
lname  = identifier lexer
lres   = reserved lexer
lspace = whiteSpace lexer


single x = [x]

notSupported x = error $ "Not supported yet: " ++ x

eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing
