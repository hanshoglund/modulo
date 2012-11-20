
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

module Language.Modulo.C ( 
        -- ** Styles
        GuardStyle(..),
        ImportStyle(..),
        CStyle(..),
        stdStyle,
        cairoStyle,
        gtkStyle,
        appleStyle, 
        haskellStyle,
        -- ** Rendering
        renderModule,
        renderModuleStyle,
        printModule,
        printModuleStyle,
  ) where

import Data.Default
import Data.Semigroup
import Language.Modulo

import Language.C.Syntax.AST
import Language.C.Pretty

-- TODO can these two be removed and defNodeInfo replaced with _|_ ?
import Language.C.Data.Node     
import Language.C.Data.Ident
-- end TODO
import Language.C.Parser
import Language.C.Data.Position
import Language.C.Data.InputStream

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty


-- Util
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
sepCase :: [String] -> String
sepCase = concatSep "_"

prefixedBy x = (x ++)
suffixedBy x = (++ x)


-------------------------------------------------------------------------------------
-- Styles
-------------------------------------------------------------------------------------

data GuardStyle 
    = Pragma -- ^ Write pragma guards
    | Ifndef -- ^ Write conditional guards
    
data ImportStyle 
    = SystemPath -- ^ Import external modules using system paths
    | LocalPath  -- ^ Import external modules using local paths
      
data CStyle =
    CStyle {   
        -- Guards
        guardStyle          :: GuardStyle,           -- ^ How to write guards
        importStyle         :: ImportStyle,          -- ^ How to write import declarations
        importDirective     :: String,               -- ^ Import directive, usually @include@.
        guardMangler        :: [String] -> String,   -- ^ Mangler for names of header guards
        
        -- Prefix
        typePrefixMangler   :: [String] -> String,   -- ^ Prefix for types
        valuePrefixMangler  :: [String] -> String,   -- ^ Prefix for values
        
        -- Types
        implStructNameMangler :: [String] -> String, -- ^ Mangler for implementation struct names
        realStructNameMangler :: [String] -> String, -- ^ Mangler for ordinary struct names
        unionNameMangler      :: [String] -> String, -- ^ Mangler for union names
        enumNameMangler       :: [String] -> String, -- ^ Mangler for enum names

        -- Fields
        structFieldMangler  :: [String] -> String,   -- ^ Mangler for struct fields
        unionFieldMangler   :: [String] -> String,   -- ^ Mangler for union fields
        enumFieldMangler    :: [String] -> String,   -- ^ Mangler for enum fields
        
        -- Functions and values
        constNameMangler    :: [String] -> String,   -- ^ Mangler for constant values
        globalNameMangler   :: [String] -> String,   -- ^ Mangler for global variables
        functionNameMangler :: [String] -> String    -- ^ Mangler for global functions
        
        -- Options
        --  Wrap in extern C block
        --  Add Doxygen stubs (which?)
        --  Add <PREFIX>_API declaration
    }

-- | Default instance using 'stdStyle'.
instance Default CStyle where
    def = stdStyle
-- | Left-biased Semigroup instance.
instance Semigroup CStyle where
    a <> b = a
-- | Left-biased Monoid instance.
instance Monoid CStyle where
    mempty  = def
    mappend = (<>)

-- | 
-- Style used in the C standard library.    
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
    Ifndef SystemPath "include"
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
    Ifndef SystemPath "include"
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
    Ifndef SystemPath "include"
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
    Ifndef SystemPath "include"
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
    Ifndef SystemPath "include"
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

-- | 
-- Render a module using the default style.
--
-- Returns a C header file, represented as a 'CTranslUnit' with enclosing header and footer strings.
--
renderModule :: Module -> (String, CTranslUnit, String)
renderModule = renderModuleStyle def

-- | 
-- Render a module using the specified style.
--
-- Returns a C header file, represented as a 'CTranslUnit' with enclosing header and footer strings.
--
renderModuleStyle :: CStyle -> Module -> (String, CTranslUnit, String)
renderModuleStyle style mod = (header, decls, footer)
    where
        header = convertHeader style mod
        footer = convertFooter style mod
        decls  = convertTopLevel style mod

-- | 
-- Print a module using the default style.
--
printModule :: Module -> String
printModule = printModuleStyle def

-- | 
-- Print a module using the specified style.
--
printModuleStyle :: CStyle -> Module -> String
printModuleStyle style = (\(x,y,z) -> x ++ (show . pretty $ y) ++ z) . renderModuleStyle style




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
    ++ imports
    ++ "\n"
    ++ "\n"
    ++ "<DECLS>"
    ++ "\n"
    ++ "\n"
    where      
        name = NonEmpty.toList . getModuleName . modName $ mod
        guard = guardMangler style name
        imports = concatSep "\n" 
            . map (prefixedBy "#include <" 
            . suffixedBy ".h>" 
            . concatSep "/"
            . NonEmpty.toList
            . getModuleName) 
            . modImports 
            $ mod
    
convertFooter :: CStyle -> Module -> String
convertFooter style mod = mempty
    ++ guardEnd (guardStyle style) guard
    ++ "\n\n"
    where
        name = NonEmpty.toList . getModuleName . modName $ mod
        guard = guardMangler style name


topDeclListElem :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
topDeclListElem declr = (Just declr, Nothing, Nothing)






convertTopLevel :: CStyle -> Module -> CTranslUnit
convertTopLevel style mod = 
     CTranslUnit [] defInfo

-- convertDecl :: CStyle -> Decl -> CDecl
-- convertType :: CStyle -> Type -> CTypeSpec
-- convertPrimType :: CStyle -> PrimType -> CTypeSpec

-- convertValue :: CStyle -> Value -> CConst ??









-- modo :: CTranslUnit
-- modo = CTranslUnit [
--     CDeclExt typ,
--     CDeclExt typ,
--     CDeclExt typ,
--     CDeclExt typ,
--     CDeclExt foo
--     ] defInfo
-- 
-- -- int x
-- field :: String -> CDecl
-- field x = CDecl [
--         CTypeSpec (CIntType defInfo)
--     ] [
--         topDeclListElem $ CDeclr (Just $ ident x) [] Nothing [] defInfo
--     ] defInfo
-- 
-- 
-- -- typedef struct _foo { int x; int y; } foo; 
-- typ :: CDecl
-- typ = CDecl [
--         CStorageSpec (CTypedef defInfo),
--         -- CTypeSpec (CTypeDef (ident "_foo") defInfo),
--         CTypeSpec (CSUType (
--             CStruct CStructTag (Just $ ident $ "_foo") (Just [field "x", field "y"]) [] defInfo
--         ) defInfo)
--     ] [
--         topDeclListElem $ CDeclr (Just $ ident "foo") [] Nothing [] defInfo,
--         topDeclListElem $ CDeclr (Just $ ident "fxx") [] Nothing [] defInfo
--     ] defInfo
-- 
-- -- static const void foo
-- foo :: CDecl
-- foo = CDecl [
--         CStorageSpec (CStatic defInfo),
--         CTypeQual (CConstQual defInfo),
--         CTypeSpec (CVoidType defInfo)
--     ] [
--         topDeclListElem $ CDeclr (Just $ ident "foo") [] Nothing [] defInfo
--     ] defInfo







-- Debug stuff


        
deriving instance Show CTranslUnit
deriving instance Show CExtDecl
deriving instance Show CStrLit
deriving instance Show CFunDef
deriving instance Show CDecl
deriving instance Show CStat
deriving instance Show CDeclr
deriving instance Show CInit
deriving instance Show CDeclSpec
deriving instance Show CAsmStmt
deriving instance Show CBlockItem
deriving instance Show CAttr
deriving instance Show CDerivedDeclr
deriving instance Show CDesignator
deriving instance Show CExpr
deriving instance Show CTypeQual
deriving instance Show CTypeSpec
deriving instance Show CAsmOperand
deriving instance Show CArrSize
deriving instance Show CBuiltin
deriving instance Show CConst
deriving instance Show CEnum
deriving instance Show CStructUnion
deriving instance Show CStructTag 



defInfo :: NodeInfo
defInfo = OnlyPos $ Position undefined 0 0

ident :: String -> Ident
ident name = Ident name 0 defInfo

