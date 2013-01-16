
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
import Data.Char (chr)
import Data.Text (pack)
import Data.AttoLisp

import Language.Modulo.C
import Language.Modulo.Util
import Language.Modulo.Util.Unmangle
import Language.Modulo

import qualified Data.List as List

-- import qualified Data.List as List
-- import qualified Data.Char as Char
-- import qualified Data.List.NonEmpty as NonEmpty

data LispStyle =
    LispStyle {
        cStyle :: CStyle,                   -- ^ For generating foreign declarations
        package :: String,                  -- ^ Package in which to generate definitions
        mangleType :: String -> String
    }

stdLispStyle = LispStyle { 
    cStyle = stdStyle,
    -- package = "cl-user",
    package = "doremir", -- TODO
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
printModuleLispStyle style = concatSep "\n" . map show . renderModuleLispStyle style
-- TODO more intelligent splitting

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
renderModuleLispStyle st = withPrefix (convertPackage st) . convertTopLevel st

convertPackage :: LispStyle -> [Lisp]
convertPackage st = [list [symbol "in-package", keyword (package st)]]

convertTopLevel :: LispStyle -> Module -> [Lisp]
convertTopLevel st (Module n is ds) = cds
    where
        cds = map (convertDecl st) ds

convertDecl :: LispStyle -> Decl -> Lisp
convertDecl st (TypeDecl n Nothing)  = declOpaque st n
convertDecl st (TypeDecl n (Just t)) = declType st n t                -- typedef T N;
convertDecl st (FunctionDecl n t)  = declFun st n t                 -- T n (as);
convertDecl st (TagDecl t)         = notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)   = notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)  = notSupported "Globals"         -- T n; or T n = v;

--
-- TODO To be more type-safe, we want to generate something like this:
-- 
-- (defclass set () ((ptr :pointer)))
-- (define-foreign-type set-type () 
--   () 
--   (:actual-type :pointer)
--   (:parse-simple set))
-- (defmethod translate-to-foreign (x (type set-type))
--   (slot-value x 'pointer))
--

declOpaque :: LispStyle -> Name -> Lisp             
declOpaque st n = list [symbol "defctype", symbolName n, keyword "pointer"]

declType :: LispStyle -> Name -> Type -> Lisp             
declType st n t = list [symbol "defctype", symbolName n, convertType st t]


-- TODO Generate
--
--   (define-foreign-type T-type () () (:actual-type :pointer))
--   (define-parse-method T () (make-instance 'T-type))

declFun :: LispStyle -> Name -> FunType -> Lisp             
declFun st n (Function as r) = list $ [
    symbol "defcfun", 
    list [name, cname],
    ret] ++ args
    where
        name        = symbolName n
        ret         = convertType st r
        cname       = string $ convertCFunName (cStyle st) n
        argNames    = map (symbol . return . chr) [97..(97+25)]
        argTypes    = map (convertType st) as                        
        args        = map (\(n,t) -> list [n, t]) (zip argNames argTypes)

    
convertType :: LispStyle -> Type -> Lisp
convertType st (AliasType n) = convertAlias st n
convertType st (PrimType t)  = convertPrimType st t
convertType st (RefType t)   = convertRefType st t
convertType st (FunType t)   = convertFunType st t
convertType st (CompType t)  = convertCompType st t

convertAlias :: LispStyle -> Name -> Lisp
convertAlias st n = symbolName n

convertPrimType :: LispStyle -> PrimType -> Lisp
convertPrimType st Bool       = keyword "boolean"
convertPrimType st Void       = keyword "void"
convertPrimType st Char       = keyword "char" 
convertPrimType st Short      = keyword "short" 
convertPrimType st Int        = keyword "int" 
convertPrimType st Long       = keyword "long" 
convertPrimType st LongLong   = keyword "long-long"
convertPrimType st UChar      = keyword "unsigned-char" 
convertPrimType st UShort     = keyword "unsigned-short" 
convertPrimType st UInt       = keyword "unsigned-int" 
convertPrimType st ULong      = keyword "unsigned-long" 
convertPrimType st ULongLong  = keyword "unsigned-long-long" 
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
-- convertPrimType st Size       = keyword "size"
-- Note: Size etc are declared in cffi-sys, unfortunately not visible to cffi
convertPrimType st Size       = keyword "int32" -- FIXME
convertPrimType st Ptrdiff    = keyword "ptrdiff"
convertPrimType st Intptr     = keyword "pointer" 
convertPrimType st UIntptr    = notSupported "Uintptr with Lisp"
convertPrimType st SChar      = notSupported "Signed chars with Lisp"


convertRefType :: LispStyle -> RefType -> Lisp
convertRefType st (Pointer t) = list [keyword "pointer", convertType st t]
convertRefType st (Array t n) = notSupported "Array types with Lisp"
-- TODO

convertFunType :: LispStyle -> FunType -> Lisp
convertFunType st (Function as r) = convertType st voidPtr

convertCompType :: LispStyle -> CompType -> Lisp
convertCompType st (Enum as)       = convertType st voidPtr
convertCompType st (Struct as)     = convertType st voidPtr
convertCompType st (Union as)      = convertType st voidPtr
convertCompType st (BitField as)   = error "Not implemented: bitfields" -- TODO








string :: String -> Lisp
string = String . pack

symbol :: String -> Lisp
symbol = Symbol . pack

keyword :: String -> Lisp
keyword x = Symbol (pack $ ":" ++ x)

stringName :: Name -> Lisp
stringName = string . convertName {- getName-}

symbolName :: Name -> Lisp
symbolName = symbol . convertName {- getName-}

keywordName :: Name -> Lisp
keywordName = keyword . convertName {- getName-}

convertName :: Name -> String
convertName (Name n)    = {-withPrefix "#" $ -} toLowerString $ concatSep "-" (unmangle n)
convertName (QName m n) = {-withPrefix "#" $ -} toLowerString $ concatSep "-" (stripPackage (getModuleNameList m) ++ unmangle n)

stripPackage :: [String] -> [String]
stripPackage = tail
-- TODO only strip prefix that matches (unmangle (package st))

convertCTypeName :: CStyle -> Name -> String
convertCTypeName st n = getName (translType st n)

convertCFunName :: CStyle -> Name -> String
convertCFunName st n = getName (translFun st n)

-- TODO type fun const global enumF structF unionF


voidPtr = RefType (Pointer $ PrimType Void)



instance Default Lisp where
    def = nil
instance Semigroup Lisp where
    (<>) = appendLisp
instance Monoid Lisp where
    mempty  = def
    mappend = (<>)

list :: [Lisp] -> Lisp
list = List

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

-- concatSep :: [a] -> [[a]] -> [a]
-- concatSep x = List.concat . List.intersperse x
