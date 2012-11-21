
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

module Language.Modulo.Lisp ( 
  ) where

import Data.Default
import Data.Semigroup
import Data.Text(pack)

import Language.Modulo
import Language.Modulo.C
import Data.AttoLisp

-- import qualified Data.List as List
-- import qualified Data.Char as Char
-- import qualified Data.List.NonEmpty as NonEmpty

data LispStyle =
    LispStyle {
        cStyle :: CStyle,                   -- ^ For generating foreign declarations
        mangleType :: String -> String
    }

-- TODO
-- CFFI can only deal with flat structs/unions
-- Can we unflatten nested struct/union types?
-- Or should we use a codegen monad to gather defcXX declarations?
    
lispType :: LispStyle -> Type -> Lisp
lispType st (AliasType n) = lispAlias st n
lispType st (PrimType t)  = lispPrimType st t
lispType st (RefType t)   = lispRefType st t
lispType st (FunType t)   = lispFunType st t
lispType st (CompType t)  = lispCompType st t

lispAlias :: LispStyle -> Name -> Lisp
lispAlias st n = keyword n --(mangleType st n)

lispPrimType :: LispStyle -> PrimType -> Lisp
lispPrimType st Void       = keyword "void"
lispPrimType st Size       = keyword (error "size-types")     -- TODO
lispPrimType st Ptrdiff    = keyword (error "size-types")
lispPrimType st Intptr     = keyword (error "size-types") 
lispPrimType st UIntptr    = keyword (error "size-types")
lispPrimType st Char       = keyword "char" 
lispPrimType st Short      = keyword "short" 
lispPrimType st Int        = keyword "int" 
lispPrimType st Long       = keyword "long" 
lispPrimType st LongLong   = keyword "long-long"
lispPrimType st UChar      = keyword "unsigned-char" 
lispPrimType st UShort     = keyword "unsigned-short" 
lispPrimType st UInt       = keyword "unsigned-int" 
lispPrimType st ULong      = keyword "unsigned-long-long" 
lispPrimType st ULongLong  = keyword "long-long"
lispPrimType st Float      = keyword "float" 
lispPrimType st Double     = keyword "double" 
lispPrimType st LongDouble = keyword "long-double"
lispPrimType st Int8       = keyword "int8" 
lispPrimType st Int16      = keyword "int16" 
lispPrimType st Int32      = keyword "int32" 
lispPrimType st Int64      = keyword "int64" 
lispPrimType st UInt8      = keyword "uint8" 
lispPrimType st UInt16     = keyword "uint16" 
lispPrimType st UInt32     = keyword "uint32" 
lispPrimType st UInt64     = keyword "uint64"
    

lispRefType :: LispStyle -> RefType -> Lisp
lispRefType st (Pointer t) = List [keyword "pointer", lispType st t]
lispRefType st (Array t n) = undefined -- TODO

lispFunType :: LispStyle -> FunType -> Lisp
lispFunType st (Function as r) = lispType st voidPtr

lispCompType :: LispStyle -> CompType -> Lisp
lispCompType st (Enum as)       = undefined
lispCompType st (Struct as)     = undefined
lispCompType st (Union as)      = undefined
lispCompType st (BitField as)   = error "Not implemented: bitfields" -- TODO



keyword :: String -> Lisp
keyword x = Symbol (pack $ ":" ++ x)

voidPtr = RefType (Pointer $ PrimType Void)

