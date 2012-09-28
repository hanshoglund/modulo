
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

data GuardStyle = Pragma | Ifndef
data ImportStyle = Angles | Quotes


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
underscoreSeparated = mconcat . List.intersperse "_" . fmap toLower

prefixedBy x = (x ++)
suffixedBy x = (++ x)


      
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


defInfo :: NodeInfo
defInfo = OnlyPos $ Position undefined 0 0

convertModule :: CStyle -> Module -> CTranslUnit
convertModule style mod = 
     CTranslUnit [] defInfo

-- convertModuleDecl :: CStyle -> ModuleDecl -> CTranslUnit
-- 
-- convertValue :: CStyle -> Value -> CDecl
-- 
-- convertType :: CStyle -> Type -> CTranslUnit
-- 
-- convertPrimType :: CStyle -> PrimType -> CTranslUnit

ident :: String -> Ident
ident name = Ident name 0 defInfo

topDeclListElem :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
topDeclListElem declr = (Just declr, Nothing, Nothing)

-- topDeclListElemInit :: CDecl
-- topDeclListElemInit declr init = (Just declr, Just init, Nothing)



modo :: CTranslUnit
modo = CTranslUnit [
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt typ,
    CDeclExt foo
    ] defInfo

-- int x
field1 :: CDecl
field1 = CDecl [
        CTypeSpec (CIntType defInfo)
    ] [
        topDeclListElem $ CDeclr (Just $ ident "foo") [] Nothing [] defInfo
    ] defInfo


-- typedef struct { int x; int y; } foo; 
typ :: CDecl
typ = CDecl [
        CStorageSpec (CTypedef defInfo),
        CTypeSpec (CSUType (
            CStruct CStructTag Nothing (Just [field1]) [] defInfo
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
printModule style = show . pretty . convertModule style
