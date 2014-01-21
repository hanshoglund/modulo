
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
        renderModuleLispStyle,
        
        -- ** Names
        convertName,
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

data LispStyle =
    LispStyle {
        cStyle          :: CStyle,                  -- ^ For generating foreign declarations
        package         :: String,                  -- ^ Package in which to generate definitions
        prefixMangler   :: [String] -> [String],    -- ^ A mangler for prefixes.
        safeOpaque      :: Bool,                    -- ^ If true, generate a wrapper class for each opaque type.
        primBoolType    :: Maybe PrimType           -- ^ Type of primitive booleans (default Int).
    }

stdLispStyle = LispStyle {       
    cStyle          = stdStyle,
    package         = "cl-user",
    prefixMangler   = tail,
    safeOpaque      = True,
    primBoolType    = Nothing
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
printModuleLispStyle style = (++ "\n\n") . concatSep "\n" . map show . renderModuleLispStyle style
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
convertTopLevel st (Module doc n is ds) = cds
    where
        cds = concatMap (convertDecl st . snd) ds

convertDecl :: LispStyle -> Decl -> [Lisp]
convertDecl st (TypeDecl n Nothing)  = declOpaque st n
convertDecl st (TypeDecl n (Just t)) = declType st n t                -- typedef T N;
convertDecl st (FunctionDecl n t)    = declFun st n t                 -- T n (as);
convertDecl st (TagDecl t)           = notSupported "Tag decls"       -- T;
convertDecl st (ConstDecl n v t)     = notSupported "Constants"       -- T n; or T n = v;
convertDecl st (GlobalDecl n v t)    = notSupported "Globals"         -- T n; or T n = v;

-- TODO Generate
--
--    (define-foreign-type T-type () () (:actual-type :pointer))
--    (define-parse-method T () (make-instance 'T-type))
--
-- OR
--    (define-foreign-type T-type () () (:actual-type :pointer) (:simple-parser T))
--
-- If safeOpaque true, also generate
--    (defclass            T () ((nat :initarg :nat)) )
--
--    (defmethod translate-to-foreign (x (type T-type))
--      (slot-value x 'nat)) 
--    (defmethod translate-from-foreign (x (type T-type))
--      (make-instance 'T :nat x))


declOpaque :: LispStyle -> Name -> [Lisp]             
declOpaque st n = [defType, defParse] ++ if (safeOpaque st) then [defClass, defInput, defOutput] else []
    where
        defType     = list [symbol "define-foreign-type", metaName, nil, nil, actual]
        actual      = list [keyword "actual-type", keyword "pointer"]
        defParse    = list [symbol "define-parse-method", typeName, nil, create]
        create      = list [symbol "make-instance", qualMetaName]
         
        defClass    = list [symbol "defclass", typeName, nil, slots]
        slots       = list [list [symbol slot, keyword "initarg", keyword slot]]
        defInput    = list [symbol "defmethod", symbol "translate-to-foreign", 
                            list [symbol "x", list [symbol "type", metaName]],
                            list [symbol "slot-value", symbol "x", symbol (withPrefix "'" slot)]]
        defOutput   = list [symbol "defmethod", symbol "translate-from-foreign",
                            list [symbol "x", list [symbol "type", metaName]],
                            list [symbol "make-instance", qualTypeName, keyword slot, symbol "x"]]

        slot            = withSuffix "-ptr" $ convertName st n
        qualMetaName    = symbol $ withPrefix "'" $ withSuffix "-type" $ convertName st n                                                       
        metaName        = symbol $ withSuffix "-type" $ convertName st n                                                       
        qualTypeName    = symbol $ withPrefix "'" $ convertName st n
        typeName        = symbol $ convertName st n
        

-- return $ list [symbol "defctype", symbolName n, keyword "pointer"]


declType :: LispStyle -> Name -> Type -> [Lisp]             
declType st n t = return $ list [symbol "defctype", symbolName st n, convertType st t]

declFun :: LispStyle -> Name -> FunType -> [Lisp]             
declFun st n (Function as r) = 
    return $ list $ [symbol "defcfun", list [name, cname], ret] ++ args
    where
        name        = symbolName st n
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
convertAlias st n = symbolName st n

convertPrimType :: LispStyle -> PrimType -> Lisp
convertPrimType st Bool       = case primBoolType st of
    Nothing             -> keyword "boolean"
    Just primBoolType   -> list [keyword "boolean", convertPrimType st primBoolType]
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
convertPrimType st Size       = keyword "int32" -- FIXME assume? -- FIXME shouldn't this be unsigned?
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
convertCompType st (Enum as)       = convertType st (PrimType Int)      -- TODO
convertCompType st (Struct as)     = convertType st voidPtr
convertCompType st (Union as)      = convertType st voidPtr
convertCompType st (BitField as)   = error "Not implemented: bitfields" -- TODO








string :: String -> Lisp
string = String . pack

symbol :: String -> Lisp
symbol = Symbol . pack

keyword :: String -> Lisp
keyword x = Symbol (pack $ ":" ++ x)

stringName :: LispStyle -> Name -> Lisp
stringName st = string . convertName st {- getName-}

symbolName :: LispStyle -> Name -> Lisp
symbolName st = symbol . convertName st {- getName-}

keywordName :: LispStyle -> Name -> Lisp
keywordName st = keyword . convertName st {- getName-}

convertName :: LispStyle -> Name -> String
convertName st (Name n)    = toLowerString $ concatSep "-" $ unmangle n
convertName st (QName m n) = toLowerString $ concatSep "-" $ (prefixMangler st) (getModuleNameList m) ++ unmangle n

convertCTypeName :: CStyle -> Name -> String
convertCTypeName st n = getName (translType st n)

convertCFunName :: CStyle -> Name -> String
convertCFunName st n = getName (translFun st n)

-- TODO type fun const global enumF structF unionF


-- TODO move
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
