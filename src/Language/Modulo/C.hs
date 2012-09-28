
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Module      : Language.Modulo
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglunds.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Language.Modulo.C ( 
  ) where

import Data.Monoid
import Language.Modulo

import Language.C.Syntax.AST
import Language.C.Pretty
import Language.C.Data.Node     -- TODO can these two be removed and defNodeInfo replaced with _|_ ?
import Language.C.Data.Ident
import Language.C.Data.Position

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty


concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . List.intersperse x

toUpper :: String -> String
toUpper = fmap Char.toUpper

toLower :: String -> String
toLower = fmap Char.toLower

toCapital :: String -> String
toCapital [] = []
toCapital (x:xs) = Char.toUpper x : toLower xs

-- fooBar
mixedCase :: [String] -> String
mixedCase [] = []
mixedCase (x:xs) = mconcat $ toLower x : fmap toCapital xs

-- FooBar
capitalCase :: [String] -> String
capitalCase = mconcat . fmap toCapital

-- foo_bar
underscoreSeparated :: [String] -> String
underscoreSeparated = mconcat . List.intersperse "_"

prefixedBy x = (x ++)
suffixedBy x = (++ x)

data GuardStyle = Pragma | Ifndef
data ImportStyle = Angles | Quotes
      
data CStyle =
    CStyle {
        guardStyle :: GuardStyle,
        importDirective :: String,
        importStyle :: ImportStyle,
        guardMangler :: [String] -> String,

        typePrefixMangler :: [String] -> String,
        valuePrefixMangler :: [String] -> String,
        
        implStructNameMangler :: [String] -> String,
        realStructNameMangler :: [String] -> String,
        unionNameMangler :: [String] -> String,
        enumNameMangler :: [String] -> String,

        structFieldMangler :: [String] -> String,
        unionFieldMangler :: [String] -> String,
        enumFieldMangler :: [String] -> String,
        
        constNameMangler :: [String] -> String,
        globalNameMangler :: [String] -> String,
        functionNameMangler :: [String] -> String
    }

-- | Style similar to Haskell/Java
haskellStyle :: CStyle
haskellStyle = CStyle
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    capitalCase
    capitalCase

    capitalCase
    capitalCase
    capitalCase
    capitalCase

    mixedCase
    mixedCase
    mixedCase
    
    mixedCase
    mixedCase
    mixedCase

-- | Style user Apple Frameworks    
appleStyle :: CStyle
appleStyle = CStyle 
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    capitalCase
    capitalCase
    
    capitalCase
    capitalCase
    capitalCase
    capitalCase

    (prefixedBy "m" . capitalCase)
    (prefixedBy "m" . capitalCase)
    (prefixedBy "m" . capitalCase)
    
    (prefixedBy "k" . capitalCase)
    (prefixedBy "g" . capitalCase)
    capitalCase

-- | Style used in GTK and many open source applications    
gtkStyle :: CStyle
gtkStyle = CStyle 
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    underscoreSeparated
    underscoreSeparated
    
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (underscoreSeparated)
    (underscoreSeparated)
    (underscoreSeparated)

    underscoreSeparated
    underscoreSeparated
    underscoreSeparated


-- Codegen


defInfo :: NodeInfo
defInfo = OnlyPos $ Position undefined 0 0

ident :: String -> Ident
ident name = Ident name 0 defInfo

topDeclListElem :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
topDeclListElem declr = (Just declr, Nothing, Nothing)

-- topDeclListElemInit :: CDecl
-- topDeclListElemInit declr init = (Just declr, Just init, Nothing)


-- Entry
convertModule :: CStyle -> Module -> (String, CTranslUnit, String)
convertModule style mod = (header, decls, footer)
    where
        header = convertHeader style mod
        footer = convertFooter style mod
        decls  = convertDecls style mod

guardBegin :: GuardStyle -> String -> String
guardBegin Pragma guard = mempty
    ++ "#pragma once\n"
guardBegin Ifndef guard = mempty
    ++ "#ifndef " ++ guard ++ "\n"
    ++ "#define " ++ guard ++ "\n"

guardEnd :: GuardStyle -> String -> String
guardEnd Pragma guard = mempty
    ++ "// " ++ guard
guardEnd Ifndef guard = mempty
    ++ "#endif // " ++ guard

convertHeader :: CStyle -> Module -> String
convertHeader style mod = mempty
    ++ "\n"
    ++ guardBegin (guardStyle style) guard
    ++ "\n"
    ++ imps
    ++ "\n"
    ++ "\n"
    ++ "<DECLS>"
    ++ "\n"
    ++ "\n"
    where      
        name = getModuleNameParts . modName $ mod
        imps = concatSep "\n" 
            . map (prefixedBy "#include <" 
            . suffixedBy ".h>" 
            . concatSep "/" 
            . getModuleNameParts) 
            . modImports 
            $ mod
        guard = guardMangler style name
    
convertFooter :: CStyle -> Module -> String
convertFooter style mod = mempty
    ++ guardEnd (guardStyle style) guard
    ++ "\n\n"
    where
        name = getModuleNameParts . modName $ mod
        guard = guardMangler style name


convertDecls :: CStyle -> Module -> CTranslUnit
convertDecls style mod = 
     CTranslUnit [] defInfo

-- convertDeclsDecl :: CStyle -> ModuleDecl -> CTranslUnit
-- 
-- convertValue :: CStyle -> Value -> CDecl
-- 
-- convertType :: CStyle -> Type -> CTranslUnit
-- 
-- convertPrimType :: CStyle -> PrimType -> CTranslUnit







modo :: CTranslUnit
modo = CTranslUnit [
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt foo
    ] defInfo

-- int x
field :: String -> CDecl
field x = CDecl [
        CTypeSpec (CIntType defInfo)
    ] [
        topDeclListElem $ CDeclr (Just $ ident x) [] Nothing [] defInfo
    ] defInfo


-- typedef struct { int x; int y; } foo; 
typ :: CDecl
typ = CDecl [
        CStorageSpec (CTypedef defInfo),
        CTypeSpec (CSUType (
            CStruct CStructTag Nothing (Just [field "x", field "y"]) [] defInfo
        ) defInfo)
    ] [
        topDeclListElem $ CDeclr (Just $ ident "foo") [] Nothing [] defInfo
    ] defInfo

foo :: CDecl
foo = CDecl [
        CStorageSpec (CStatic defInfo),
        CTypeQual (CConstQual defInfo),
        CTypeSpec (CVoidType defInfo)
    ] [
        topDeclListElem $ CDeclr (Just $ ident "foo") [] Nothing [] defInfo
    ] defInfo





printModule :: CStyle -> Module -> String
printModule style = (\(x,y,z) -> x ++ (show . pretty $ y) ++ z) . convertModule style



-- module Foo
--     module Bar
--         import X.Y.Z.Baz
--         
--         struct Note { pitch : Pitch }
--         enum Pitch { C, D, E }
--         
--         foo = 5 : Int
-- 
--         foo : Note -> Note
--         bar : Pitch -> Pitch
testModule = 
    Module (ModuleName $ NonEmpty.fromList ["scl", "data", "list"]) 
        [
            ModuleName $ NonEmpty.fromList ["scl", "data", "ref"],
            ModuleName $ NonEmpty.fromList ["scl", "data", "string"]
        ]
        [   
            TypeDecl "Note" (Struct $ NonEmpty.fromList [("pitch", Ref "Pitch")]),
            TypeDecl "Pitch" (Enum $ NonEmpty.fromList ["C", "D", "E"]),
            GlobalDecl "foo" (Just 5) (PrimType Int)
        ]
