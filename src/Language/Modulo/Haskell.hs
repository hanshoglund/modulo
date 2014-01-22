
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
-- Renders module descriptions as Haskell 2010 foreign declarations.
--
-------------------------------------------------------------------------------------

{-                                          
    module (...) where
    data FaeString
    foreign import ccall "fae_fae_version_string"            
        c_VersionString :: IO (Ptr FaeString)

    
-}

module Language.Modulo.Haskell ( 
        -- ** Styles
        HaskellStyle(..),
        stdHaskellStyle,
        -- ** Rendering
        printModuleHaskell,
        renderModuleHaskell,
        printModuleHaskellStyle,
        renderModuleHaskellStyle
  ) where

import Data.Default
import Data.Foldable (toList)
import Data.Semigroup
import Data.Char (chr)
import Data.Text (pack)
import Data.String
import Language.Haskell.Syntax hiding (Module)
import Language.Haskell.Pretty

import Language.Modulo.C
import Language.Modulo.Util
import Language.Modulo.Util.Unmangle
import Language.Modulo

import qualified Data.List as List
import qualified Language.Haskell.Syntax as Hs

data HaskellStyle =
    HaskellStyle {
        cStyle :: CStyle                    -- ^ For generating foreign declarations
    }

stdHaskellStyle = HaskellStyle {       
    cStyle = stdStyle
    }

-- | Default instance using 'stdStyle'.
instance Default HaskellStyle where
    def = stdHaskellStyle
-- | Left-biased Semigroup instance.
instance Semigroup HaskellStyle where
    a <> b = a
-- | Left-biased Monoid instance.
instance Monoid HaskellStyle where
    mempty  = def
    mappend = (<>)

-- |
-- Print a module using the default style.
--
printModuleHaskell :: Module -> String
printModuleHaskell = printModuleHaskellStyle def

-- |
-- Print a module using the specified style.
--
printModuleHaskellStyle :: HaskellStyle -> Module -> String
printModuleHaskellStyle style = (++ "\n\n") . prettyPrint . renderModuleHaskellStyle style

-- |
-- Render a module using the default style.
--
-- Returns a Haskell file, represented as a syntax tree.
--
renderModuleHaskell :: Module -> HsModule
renderModuleHaskell = renderModuleHaskellStyle def

-- |
-- Render a module using the specified style.
--
-- Returns a Haskell file, represented as a syntax tree.
--
renderModuleHaskellStyle :: HaskellStyle -> Module -> HsModule
renderModuleHaskellStyle st = convertTopLevel st

convertTopLevel :: HaskellStyle -> Module -> HsModule
convertTopLevel st (Module n opt doc is ds) = 
    -- TODO docs
    -- TODO nice export spec (excluding opaque struct types etc)
    HsModule def (convertModule n) Nothing imps  decls
    where
        imps  = standardForeignImports ++ concatMap (uncurry convertImport) is
        decls = concatMap (convertDecl st . snd) ds

convertImport :: ModuleName -> Maybe String -> [HsImportDecl]
convertImport _ (Just "C")  = [] -- Skip C imports
convertImport n _           = [HsImportDecl def (convertModule n) False Nothing Nothing]

standardForeignImports = [
    HsImportDecl def (Hs.Module "Foreign")   False Nothing Nothing,
    HsImportDecl def (Hs.Module "Foreign.C") False Nothing Nothing]

convertDecl :: HaskellStyle -> Decl -> [HsDecl]
convertDecl st (TypeDecl n Nothing)  = declOpaque st n
-- Uses return here as only opaque needs to emit an extra declaration
-- We might change the types of declType, declFun etc instead
convertDecl st (TypeDecl n (Just t)) = return $ declType st n t                -- typedef T N;
convertDecl st (FunctionDecl n t)    = return $ declFun st n t                 -- T n (as);
convertDecl st (TagDecl t)           = return $ notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)     = return $ notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)    = return $ notSupported "Globals"         -- T n; or T n = v;
 
declOpaque :: HaskellStyle -> Name -> [HsDecl]             
declOpaque st (Name n)    = error "Expected qualified name"
declOpaque st (QName _ n) = [HsDataDecl def [] (HsIdent $ n ++ "_") [] [] [], 
    HsTypeDecl def (HsIdent $ n) [] $
        HsTyCon (UnQual "Ptr") `HsTyApp` HsTyCon (UnQual (HsIdent $ n ++ "_"))]

declType :: HaskellStyle -> Name -> Type -> HsDecl             
declType st n t = HsTypeDecl def (HsIdent $ getNameEnd n) [] (convertType st t)

-- TODO check purity properties and add IO if needed
declFun :: HaskellStyle -> Name -> FunType -> HsDecl             
declFun st n t = HsForeignImport def "ccall" HsUnsafe cName hsName (addIO hsType)
    where
        cName   = getName (translFun (cStyle st) n) -- Always returns an unqualified name (TODO document in C module)
        hsName  = HsIdent $ getNameEnd n
        hsType  = convertFunType st t

addIO (HsTyFun a b) = HsTyFun a (addIO b)
addIO b             = HsTyApp (HsTyVar $ HsIdent "IO") b

-- TODO move
-- | Returns the last part of an unqualified name.
getNameEnd (QName _ x) = x
getNameEnd _ = error "Expected qualified name"

-- TODO partial on (CompType (Struct..)), (for struct, union and bitfield)
convertType :: HaskellStyle -> Type -> HsType
convertType st (AliasType n) = convertAlias st n
convertType st (PrimType t)  = convertPrimType st t
convertType st (RefType t)   = convertRefType st t
convertType st (FunType t)   = convertFunType st t
convertType st (CompType t)  = convertCompType st t

convertAlias :: HaskellStyle -> Name -> HsType 
convertAlias st n = HsTyCon $ UnQual $ HsIdent $ getNameEnd n
-- convertAlias st (Name n)    = HsTyCon $ (UnQual (HsIdent n))
-- convertAlias st (QName m n) = HsTyCon $ (Qual (convertModule m) (HsIdent n))

convertPrimType :: HaskellStyle -> PrimType -> HsType
convertPrimType st Bool       = HsTyCon (UnQual "CInt")
convertPrimType st Void       = unit_tycon
convertPrimType st Char       = HsTyCon (UnQual "CChar") 
convertPrimType st Short      = HsTyCon (UnQual "CShort") 
convertPrimType st Int        = HsTyCon (UnQual "CInt") 
convertPrimType st Long       = HsTyCon (UnQual "CLong") 
convertPrimType st LongLong   = notSupported "long long with Haskell"
convertPrimType st UChar      = HsTyCon (UnQual "CUChar") 
convertPrimType st UShort     = HsTyCon (UnQual "CUShort") 
convertPrimType st UInt       = HsTyCon (UnQual "CUInt") 
convertPrimType st ULong      = HsTyCon (UnQual "CULong") 
convertPrimType st ULongLong  = notSupported "(unsigned) long long with Haskell" 
convertPrimType st Float      = HsTyCon (UnQual "CFloat") 
convertPrimType st Double     = HsTyCon (UnQual "CDouble") 
convertPrimType st LongDouble = notSupported "long double with Haskell"
convertPrimType st Int8       = HsTyCon (UnQual "Int8") 
convertPrimType st Int16      = HsTyCon (UnQual "Int16") 
convertPrimType st Int32      = HsTyCon (UnQual "Int32") 
convertPrimType st Int64      = HsTyCon (UnQual "Int64") 
convertPrimType st UInt8      = HsTyCon (UnQual "Word8") 
convertPrimType st UInt16     = HsTyCon (UnQual "Word16") 
convertPrimType st UInt32     = HsTyCon (UnQual "Word32") 
convertPrimType st UInt64     = HsTyCon (UnQual "Word64")
convertPrimType st Size       = HsTyCon (UnQual "CSize")
convertPrimType st Ptrdiff    = HsTyCon (UnQual "CPtrdiff")
convertPrimType st Intptr     = HsTyCon (UnQual "CIntPtr") 
convertPrimType st UIntptr    = notSupported "Uintptr with Haskell"
convertPrimType st SChar      = notSupported "Signed chars with Haskell"

convertRefType :: HaskellStyle -> RefType -> HsType
convertRefType st (Pointer t) = HsTyCon (UnQual "Ptr") `HsTyApp` convertType st t
convertRefType st (Array t n) = notSupported "Array types with Haskell"
-- -- TODO

convertFunType :: HaskellStyle -> FunType -> HsType
convertFunType st = go
    where
        go (Function []     r) = {-(HsTyCon (UnQual "IO")) `HsTyApp`-} convertType st r
        go (Function ((_,t):ts) r) = convertType st t `HsTyFun` convertFunType st (Function ts r)
        -- TODO #34 use param names

convertCompType :: HaskellStyle -> CompType -> HsType
convertCompType st (Enum as)       = HsTyCon (UnQual "CInt")
convertCompType st (Struct as)     = convertType st voidPtr
convertCompType st (Union as)      = convertType st voidPtr
convertCompType st (BitField as)   = notSupported "Haskell: bitfields"

instance IsString HsName where
    fromString = HsIdent

instance IsString Hs.Module where
    fromString = Hs.Module

instance Default SrcLoc where
    def = SrcLoc "" 0 0

convertModule :: ModuleName -> Hs.Module
convertModule = Hs.Module . concatSep "." . getModuleNameList

notSupported x = error $ "Not supported yet: " ++ x

-- TODO move
voidPtr = RefType (Pointer $ PrimType Void)
