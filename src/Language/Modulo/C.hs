
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
-- Renders module descriptions as C header files.
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
import Language.C.Syntax.Constants
import Language.C.Parser
import Language.C.Pretty
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.InputStream

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty

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

-- | Default instance using 'stdStyle'.
instance Default CStyle where
    def = stdStyle
-- | Left-biased Semigroup instance.
instance Semigroup CStyle where
    a <> b = a
-- |Left-biased Monoid instance.
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
    (withPrefix "_" . concatSep "_" . fmap toUpper)
    (concatSep "_")
    (concatSep "_")

    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
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
    (withPrefix "_" . concatSep "_" . fmap toUpper)
    (concatSep "_")
    (concatSep "_")

    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
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
    (withPrefix "_" . concatSep "_" . fmap toUpper)
    (concatSep "_")
    (concatSep "_")

    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
    (withSuffix "_t" . concatSep "_")
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
    (withPrefix "_" . concatSep "_" . fmap toUpper)
    capitalCase
    capitalCase

    capitalCase
    capitalCase
    capitalCase
    capitalCase

    (withPrefix "m" . capitalCase)
    (withPrefix "m" . capitalCase)
    (withPrefix "m" . capitalCase)

    (withPrefix "k" . capitalCase)
    (withPrefix "g" . capitalCase)
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
    (withPrefix "_" . concatSep "_" . fmap toUpper)
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



-------------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------------

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
        decls  = convertTopLevel style mod
        footer = convertFooter style mod


-------------------------------------------------------------------------------------
-- Header and footer

convertHeader :: CStyle -> Module -> String
convertHeader st mod = mempty
    ++ "\n"
    ++ guardBegin (guardStyle st) guard
    ++ "\n"
    ++ imports
    ++ "\n"
    ++ "\n"
    where
        name = NonEmpty.toList . getModuleName . modName $ mod
        guard = guardMangler st name
        imports = concatSep "\n"
            . map (withPrefix ("#" ++ importDirective st ++ " <") . withSuffix ".h>" . concatSep "/" . NonEmpty.toList . getModuleName)
            . modImports
            $ mod

guardBegin :: GuardStyle -> String -> String
guardBegin Pragma name = mempty
    ++ "#pragma once\n"
guardBegin Ifndef name = mempty
    ++ "#ifndef " ++ name ++ "\n"
    ++ "#define " ++ name ++ "\n"

guardEnd :: GuardStyle -> String -> String
guardEnd Pragma name = mempty
    ++ "// " ++ name
guardEnd Ifndef name = mempty
    ++ "#endif // " ++ name

convertFooter :: CStyle -> Module -> String
convertFooter st mod = mempty
    ++ "\n\n"
    ++ guardEnd (guardStyle st) guard
    ++ "\n\n"
    where
        name = NonEmpty.toList . getModuleName . modName $ mod
        guard = guardMangler st name




-------------------------------------------------------------------------------------
-- Top-level declarations

convertTopLevel :: CStyle -> Module -> CTranslUnit
convertTopLevel st (Module n is ds) = CTranslUnit cds defInfo
    where
        cds = map (CDeclExt . convertDecl st) ds

convertDecl :: CStyle -> Decl -> CDecl
convertDecl st (TypeDecl n t)      = declTypeDef st n t             -- typedef T N;
convertDecl st (FunctionDecl n t)  = notSupported "Function decls"  -- T n (as);
convertDecl st (TagDecl t)         = notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)   = notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)  = notSupported "Globals"         -- T n; or T n = v;

declTypeDef :: CStyle -> Name -> Type -> CDecl             
declTypeDef st n t = CDecl spec decList defInfo
    where
        (typ, decl) = convertType st t
        spec    = [CStorageSpec (CTypedef defInfo)] ++ map CTypeSpec typ
        declr   = CDeclr (Just $ ident n) decl Nothing [] defInfo
        decList = [topLevelDeclr declr]



-- These are used recursively from below

declParam :: CStyle -> Maybe Name -> Type -> CDecl
declParam st n t = CDecl spec decList defInfo
    where
        (typ, decl) = convertType st t
        spec    = map CTypeSpec typ
        declr   = CDeclr (fmap ident n) decl Nothing [] defInfo
        decList = [memberDeclr declr]

-- struct or union member
declStructMember :: CStyle -> Name -> Type -> CDecl
declStructMember st n t = CDecl spec decList defInfo
    where
        (typ, decl) = convertType st t
        spec    = map CTypeSpec typ
        declr   = CDeclr (Just $ ident n) decl Nothing [] defInfo
        decList = [memberDeclr declr]

declBitfieldMember :: CStyle -> Maybe Name -> Type -> Int -> CDecl
declBitfieldMember = notSupported "Bit-fields"

-- | 
-- C types are represented by
-- 
--     1) A sequence of (declaration-specific) type specifiers such as 'unsigned', 'long', 'int' etc 
--
--     2) A sequence of (variable-specific) qualifiers such as '*', '[]' or function arguments 
--
convertType :: CStyle -> Type -> ([CTypeSpec], [CDerivedDeclr])
convertType st (AliasType n) = convertAlias st n
convertType st (PrimType t)  = convertPrimType st t
convertType st (RefType t)   = convertRefType st t
convertType st (FunType t)   = convertFunType st t
convertType st (CompType t)  = convertCompType st t

convertAlias :: CStyle -> Name -> ([CTypeSpec], [CDerivedDeclr])
convertAlias st n = (alias, [])
    where
        alias = [CTypeDef (ident n) defInfo]

convertPrimType :: CStyle -> PrimType -> ([CTypeSpec], [CDerivedDeclr])
convertPrimType st t = (prim t, [])
    where
        prim Void       = [CVoidType defInfo]
        prim Char       = [CCharType defInfo]
        prim Short      = [CShortType defInfo] 
        prim Int        = [CIntType defInfo] 
        prim Long       = [CLongType defInfo]
        prim LongLong   = [CLongType defInfo, CLongType defInfo]
        prim SChar      = [CSignedType defInfo, CCharType defInfo]
        prim UChar      = [CUnsigType defInfo, CCharType defInfo]
        prim UShort     = [CUnsigType defInfo, CShortType defInfo]
        prim UInt       = [CUnsigType defInfo, CIntType defInfo]
        prim ULong      = [CUnsigType defInfo, CLongType defInfo]
        prim ULongLong  = [CUnsigType defInfo, CLongType defInfo, CLongType defInfo]
        prim Float      = [CFloatType defInfo]
        prim Double     = [CDoubleType defInfo]
        prim LongDouble = [CLongType defInfo, CDoubleType defInfo]
        prim Size       = return $ CTypeDef (ident "size_t") defInfo
        prim Ptrdiff    = return $ CTypeDef (ident "ptrdiff_t") defInfo
        prim Intptr     = return $ CTypeDef (ident "intptr_t") defInfo 
        prim UIntptr    = return $ CTypeDef (ident "uintptr_t") defInfo
        prim Int8       = return $ CTypeDef (ident "int8_t") defInfo
        prim Int16      = return $ CTypeDef (ident "int16_t") defInfo
        prim Int32      = return $ CTypeDef (ident "int32_t") defInfo
        prim Int64      = return $ CTypeDef (ident "int64_t") defInfo
        prim UInt8      = return $ CTypeDef (ident "uint8_t") defInfo
        prim UInt16     = return $ CTypeDef (ident "uint16_t") defInfo
        prim UInt32     = return $ CTypeDef (ident "uint32_t") defInfo
        prim UInt64     = return $ CTypeDef (ident "uint64_t") defInfo



convertRefType :: CStyle -> RefType -> ([CTypeSpec], [CDerivedDeclr])
convertRefType st (Pointer t) = (typ, [CPtrDeclr [] defInfo] ++ decls)
    where
        (typ, decls) = convertType st t
convertRefType st (Array t n) = (typ, [CArrDeclr [] size defInfo] ++ decls)
    where
        size = (CArrSize isStatic (CConst (CIntConst (fromIntegral n) defInfo)))
        isStatic = False
        (typ, decls) = convertType st t

convertFunType :: CStyle -> FunType -> ([CTypeSpec], [CDerivedDeclr])
convertFunType st (Function as r) = (typ, [CFunDeclr (Right (args, False)) [] defInfo ] ++ decls)
    where                                                             
        (typ, decls) = convertType st r
        args :: [CDecl]
        args    = map (\t -> declParam st Nothing t) as

convertCompType :: CStyle -> CompType -> ([CTypeSpec], [CDerivedDeclr])
convertCompType st (Enum as) = ([typ], [])
    where                            
        typ     = CEnumType enum defInfo
        enum    = CEnum tag (Just names) [] defInfo
        tag     = Nothing
        names   = map (\n -> (ident n, Nothing)) $ NonEmpty.toList as
        
convertCompType st (Struct as) = ([typ], [])
    where                              
        typ     = CSUType struct defInfo
        struct  = cStruct tag (Just decls) [] defInfo
        tag     = Nothing
        decls   = map (\(n,t) -> declStructMember st n t) $ NonEmpty.toList as

convertCompType st (Union as) = ([typ], [])
    where                              
        typ     = CSUType union defInfo
        union   = cUnion tag (Just decls) [] defInfo
        tag     = Nothing
        decls   = map (\(n,t) -> declStructMember st n t) $ NonEmpty.toList as

convertCompType st (BitField as) = notSupported "Bitfields"











-------------------------------------------------------------------------------------
-- Util

-- | An identifier
ident :: String -> Ident
ident name = Ident name 0 defInfo

-- | Top-level declaration.
topLevelDeclr :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
topLevelDeclr declr = (Just declr, Nothing, Nothing)

-- | Top-level declaration with initial value.
topLevelDeclrInit :: CDeclr -> CInit -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
topLevelDeclrInit declr init = (Just declr, Just init, Nothing)

-- | Member declaration.
memberDeclr :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
memberDeclr declr = (Just declr, Nothing, Nothing)

-- | Member declaration with size.
memberDeclrSize :: CDeclr -> CExpr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
memberDeclrSize declr size = (Just declr, Nothing, Just size)

-- | Parameter declaration.
paramDeclr :: CDeclr -> (Maybe CDeclr, Maybe CInit, Maybe CExpr)
paramDeclr declr = (Just declr, Nothing, Nothing)


cStruct :: Maybe Ident -> Maybe [CDecl] -> [CAttr] -> NodeInfo -> CStructUnion
cStruct = CStruct CStructTag

cUnion :: Maybe Ident -> Maybe [CDecl] -> [CAttr] -> NodeInfo -> CStructUnion
cUnion  = CStruct CUnionTag

-- | Used for all NodeInfo values in generated code
defInfo :: NodeInfo
defInfo = error "Can not read nodeInfo"
-- defInfo = OnlyPos $ Position undefined 0 0

notSupported x = error $ "Not supported yet: " ++ x

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

instance Num CInteger where
    (CInteger a r f) + (CInteger b _ _) = CInteger (a+b) r f
    (CInteger a r f) * (CInteger b _ _) = CInteger (a*b) r f
    abs (CInteger a r f)                = CInteger (abs a) r f
    signum (CInteger a r f)             = CInteger (signum a) r f
    fromInteger a                       = CInteger a DecRepr noFlags


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

withPrefix :: [a] -> [a] -> [a]
withPrefix x = (x ++)

withSuffix :: [a] -> [a] -> [a]
withSuffix x = (++ x)

