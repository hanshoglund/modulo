
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
        GuardStyle(..),
        ImportStyle(..),
        CStyle(..),
        stdStyle,
        cairoStyle,
        gtkStyle,
        appleStyle, 
        haskellStyle,
        convertModule,
        printModule,
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
usSep :: [String] -> String
usSep = concatSep "_"

prefixedBy x = (x ++)
suffixedBy x = (++ x)

data GuardStyle = Pragma | Ifndef
data ImportStyle = Angles | Quotes
      
data CStyle =
    CStyle {   
        -- Guards
        guardStyle          :: GuardStyle,           -- ^ How to write guards
        importStyle         :: ImportStyle,          -- ^ What character to use for imports
        importDirective     :: String,               -- ^ Import directive, usually @include@.
        guardMangler        :: [String] -> String,   -- ^ Mangles names of header guards
        
        -- Prefix
        typePrefixMangler   :: [String] -> String,   -- ^ Prefix for types
        valuePrefixMangler  :: [String] -> String,   -- ^ Prefix for values
        
        -- Types
        implStructNameMangler :: [String] -> String, -- ^ Mangles implementation struct names
        realStructNameMangler :: [String] -> String, -- ^ Mangles ordinary struct names
        unionNameMangler      :: [String] -> String, -- ^ Mangles union names
        enumNameMangler       :: [String] -> String, -- ^ Mangles enum names

        -- Fields
        structFieldMangler  :: [String] -> String,   -- ^ Mangles struct fields
        unionFieldMangler   :: [String] -> String,   -- ^ Mangles union fields
        enumFieldMangler    :: [String] -> String,   -- ^ Mangles enum fields
        
        -- Functions and values
        constNameMangler    :: [String] -> String,   -- ^ Mangles constant values
        globalNameMangler   :: [String] -> String,   -- ^ Mangles global variables
        functionNameMangler :: [String] -> String    -- ^ Mangles global functions
    }

-- | 
-- Style used in the standard library.    
--
-- * Types:     @ pfoobar_t @ 
--
-- * Opaques:   @ _pfoobar_t @ 
--
-- * Functions: @ p_foo_bar @ 
--
-- * Constants: @ P_FOO_BAR @ 
--
-- * Fields:    @ foo_bar @
stdStyle :: CStyle
stdStyle = CStyle 
    Ifndef Angles "include"
    (prefixedBy "_" . concat . fmap toUpper)
    (concatSep "_")
    (concatSep "_")
    
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (concatSep "_")
    (concatSep "_")
    (concatSep "_")

    (concatSep "_")
    (concatSep "_")
    (concatSep "_")

-- | 
-- Style used in Cairo.    
--
-- * Types:     @ p_foo_bar_t @ 
--
-- * Opaques:   @ _p_foo_bar_t @ 
--
-- * Functions: @ p_foo_bar @ 
--
-- * Constants: @ P_FOO_BAR @ 
--
-- * Fields:    @ foo_bar @
cairoStyle :: CStyle
cairoStyle = CStyle 
    Ifndef Angles "include"
    (prefixedBy "_" . concat . fmap toUpper)
    (concatSep "_")
    (concatSep "_")
    
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (concatSep "_")
    (concatSep "_")
    (concatSep "_")

    (concatSep "_")
    (concatSep "_")
    (concatSep "_")


-- | 
-- Style used in GTK.    
--
-- * Types:     @ PFooBar @ 
--
-- * Opaques:   @ _PFooBar @ 
--
-- * Functions: @ p_foo_bar @ 
--
-- * Constants: @ P_FOO_BAR @ 
--
-- * Fields:    @ foo_bar @
gtkStyle :: CStyle
gtkStyle = CStyle 
    Ifndef Angles "include"
    (prefixedBy "_" . concatSep "_" . fmap toUpper)
    (concatSep "_")
    (concatSep "_")
    
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (suffixedBy "_t" . concatSep "_")
    (concatSep "_")
    (concatSep "_")
    (concatSep "_")

    (concatSep "_")
    (concatSep "_")
    (concatSep "_")

-- | 
-- Style used in Apple Frameworks.    
--
-- * Types:     @ PFooBar @ 
--
-- * Opaques:   @ PFooBarOpaque @ 
--
-- * Functions: @ PFooBar @ 
--
-- * Constants: @ kPFooBar @ 
--
-- * Fields:    @ mFooBar @
appleStyle :: CStyle
appleStyle = CStyle 
    Ifndef Angles "include"
    (prefixedBy "_" . concatSep "_" . fmap toUpper)
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

-- | 
-- Style similar to Haskell conventions.
--
-- * Types:     @ PFooBar @ 
--
-- * Opaques:   @ PFooBarOpaque @ 
--
-- * Functions: @ pfooBar @ 
--
-- * Constants: @ pfooBar @ 
--
-- * Fields:    @ pfooBar @
haskellStyle :: CStyle
haskellStyle = CStyle
    Ifndef Angles "include"
    (prefixedBy "_" . concatSep "_" . fmap toUpper)
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
        decls  = convertTopLevel style mod

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


convertTopLevel :: CStyle -> Module -> CTranslUnit
convertTopLevel style mod = 
     CTranslUnit [] defInfo

-- convertDecl :: CStyle -> ModuleDecl -> CDecl


-- convertValue :: CStyle -> Value -> CDecl
-- convertType :: CStyle -> Type -> ?
-- convertPrimType :: CStyle -> PrimType -> ?







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
