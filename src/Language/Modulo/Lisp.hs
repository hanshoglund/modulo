
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
printModuleLispStyle style = show . renderModuleLispStyle style

-- |
-- Render a module using the default style.
--
-- Returns a C header file, represented as a 'CTranslUnit' with enclosing header and footer strings.
--
renderModuleLisp :: Module -> Lisp
renderModuleLisp = renderModuleLispStyle def

-- |
-- Render a module using the specified style.
--
-- Returns a C header file, represented as a 'CTranslUnit' with enclosing header and footer strings.
--
renderModuleLispStyle :: LispStyle -> Module -> Lisp
renderModuleLispStyle style mod = nil


convertTopLevel :: LispStyle -> Module -> Lisp
convertTopLevel st (Module n is ds) = mconcat cds
    where
        cds = map (convertDecl st) ds

convertDecl :: LispStyle -> Decl -> Lisp
convertDecl st (TypeDecl n t)      = undefined                      -- typedef T N;
convertDecl st (FunctionDecl n t)  = undefined                      -- T n (as);
convertDecl st (TagDecl t)         = notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)   = notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)  = notSupported "Globals"         -- T n; or T n = v;




-- TODO
-- CFFI can only deal with flat structs/unions
-- Can we unflatten nested struct/union types?
-- Or should we use a codegen monad to gather defcXX declarations?
-- Or just fail and let the user rewrite manually
    
convertType :: LispStyle -> Type -> Lisp
convertType st (AliasType n) = lispAlias st n
convertType st (PrimType t)  = lispPrimType st t
convertType st (RefType t)   = convertRefType st t
convertType st (FunType t)   = convertFunType st t
convertType st (CompType t)  = convertCompType st t

lispAlias :: LispStyle -> Name -> Lisp
lispAlias st n = keyword n

lispPrimType :: LispStyle -> PrimType -> Lisp
lispPrimType st Void       = keyword "void"
lispPrimType st Bool       = keyword "bool"
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
-- CFFI does not support these 
lispPrimType st Size       = error "Can not use size types with CFFI"
lispPrimType st Ptrdiff    = error "Can not use size types with CFFI"
lispPrimType st Intptr     = error "Can not use size types with CFFI" 
lispPrimType st UIntptr    = error "Can not use size types with CFFI"
lispPrimType st SChar      = error "Can not use signed char types with CFFI" 

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
