
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
-- Renders module descriptions as Common Lisp (CFFI) declarations.
--
-------------------------------------------------------------------------------------

module Language.Modulo.Lisp (
        -- ** Styles
        LispStyle(..),
        stdLispStyle,
        -- ** Rendering
        printModuleLisp,
        renderModuleLisp,
        printModuleLispStyle,
        renderModuleLispStyle
  ) where

import Data.Default
import Data.Semigroup
import Data.Text(pack)
import Data.AttoLisp

import Language.Modulo.C
import Language.Modulo

import qualified Data.List as List

-- import qualified Data.List as List
-- import qualified Data.Char as Char
-- import qualified Data.List.NonEmpty as NonEmpty

data LispStyle =
    LispStyle {
        cStyle :: CStyle,                   -- ^ For generating foreign declarations
        mangleType :: String -> String
    }

stdLispStyle = LispStyle { 
    cStyle = stdStyle,
    mangleType = id
    }

-- | Default instance using 'stdStyle'.
instance Default LispStyle where
    def = stdLispStyle
-- | Left-biased Semigroup instance.
instance Semigroup LispStyle where
    a <> b = a
-- | Left-biased Monoid instance.
instance Monoid LispStyle where
    mempty  = def
    mappend = (<>)

-- |
-- Print a module using the default style.
--
printModuleLisp :: Module -> String
printModuleLisp = printModuleLispStyle def

-- |
-- Print a module using the specified style.
--
printModuleLispStyle :: LispStyle -> Module -> String
printModuleLispStyle style = concatSep "\n\n" . map show . renderModuleLispStyle style

-- |
-- Render a module using the default style.
--
-- Returns a Lisp file, represented as a sequence of S-expressions.
--
renderModuleLisp :: Module -> [Lisp]
renderModuleLisp = renderModuleLispStyle def

-- |
-- Render a module using the specified style.
--
-- Returns a Lisp file, represented as a sequence of S-expressions.
--
renderModuleLispStyle :: LispStyle -> Module -> [Lisp]
renderModuleLispStyle = convertTopLevel


convertTopLevel :: LispStyle -> Module -> [Lisp]
convertTopLevel st (Module n is ds) = cds
    where
        cds = map (convertDecl st) ds

convertDecl :: LispStyle -> Decl -> Lisp
convertDecl st (TypeDecl n t)      = declType st n t                -- typedef T N;
convertDecl st (FunctionDecl n t)  = declFun st n t                 -- T n (as);
convertDecl st (TagDecl t)         = notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)   = notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)  = notSupported "Globals"         -- T n; or T n = v;

declType :: LispStyle -> Name -> Type -> Lisp             
declType st n t = List [symbol "defctype", symbol n, convertType st t]

declFun :: LispStyle -> Name -> FunType -> Lisp             
declFun st n (Function as r) = List [symbol "defcfun", symbol n, ret]
    where
        ret = convertType st r
        args = []





-- TODO
-- CFFI can only deal with flat structs/unions
-- Can we unflatten nested struct/union types?
-- Or should we use a codegen monad to gather defcXX declarations?
-- Or just fail and let the user rewrite manually
    
convertType :: LispStyle -> Type -> Lisp
convertType st (AliasType n) = convertAlias st n
convertType st (PrimType t)  = convertPrimType st t
convertType st (RefType t)   = convertRefType st t
convertType st (FunType t)   = convertFunType st t
convertType st (CompType t)  = convertCompType st t

convertAlias :: LispStyle -> Name -> Lisp
convertAlias st n = keyword n

convertPrimType :: LispStyle -> PrimType -> Lisp
convertPrimType st Void       = keyword "void"
convertPrimType st Bool       = keyword "bool"
convertPrimType st Char       = keyword "char" 
convertPrimType st Short      = keyword "short" 
convertPrimType st Int        = keyword "int" 
convertPrimType st Long       = keyword "long" 
convertPrimType st LongLong   = keyword "long-long"
convertPrimType st UChar      = keyword "unsigned-char" 
convertPrimType st UShort     = keyword "unsigned-short" 
convertPrimType st UInt       = keyword "unsigned-int" 
convertPrimType st ULong      = keyword "unsigned-long-long" 
convertPrimType st ULongLong  = keyword "long-long"
convertPrimType st Float      = keyword "float" 
convertPrimType st Double     = keyword "double" 
convertPrimType st LongDouble = keyword "long-double"
convertPrimType st Int8       = keyword "int8" 
convertPrimType st Int16      = keyword "int16" 
convertPrimType st Int32      = keyword "int32" 
convertPrimType st Int64      = keyword "int64" 
convertPrimType st UInt8      = keyword "uint8" 
convertPrimType st UInt16     = keyword "uint16" 
convertPrimType st UInt32     = keyword "uint32" 
convertPrimType st UInt64     = keyword "uint64"
-- CFFI does not support these 
convertPrimType st Size       = error "Can not use size types with CFFI"
convertPrimType st Ptrdiff    = error "Can not use size types with CFFI"
convertPrimType st Intptr     = error "Can not use size types with CFFI" 
convertPrimType st UIntptr    = error "Can not use size types with CFFI"
convertPrimType st SChar      = error "Can not use signed char types with CFFI" 

convertRefType :: LispStyle -> RefType -> Lisp
convertRefType st (Pointer t) = List [keyword "pointer", convertType st t]
convertRefType st (Array t n) = convertType st voidPtr

convertFunType :: LispStyle -> FunType -> Lisp
convertFunType st (Function as r) = convertType st voidPtr

convertCompType :: LispStyle -> CompType -> Lisp
convertCompType st (Enum as)       = convertType st voidPtr
convertCompType st (Struct as)     = convertType st voidPtr
convertCompType st (Union as)      = convertType st voidPtr
convertCompType st (BitField as)   = error "Not implemented: bitfields" -- TODO


symbol :: String -> Lisp
symbol = Symbol . pack

keyword :: String -> Lisp
keyword x = Symbol (pack $ ":" ++ x)

voidPtr = RefType (Pointer $ PrimType Void)



instance Default Lisp where
    def = nil
instance Semigroup Lisp where
    (<>) = appendLisp
instance Monoid Lisp where
    mempty  = def
    mappend = (<>)

single :: Lisp -> Lisp
single a = List [a]

appendLisp :: Lisp -> Lisp -> Lisp
appendLisp a b = List (as ++ bs)
    where
        (List as) = assureList a
        (List bs) = assureList b

assureList :: Lisp -> Lisp
assureList (List as)      = List as
assureList (DotList as a) = DotList as a
assureList x              = List [x]

notSupported x = error $ "Not supported yet: " ++ x

concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . List.intersperse x
