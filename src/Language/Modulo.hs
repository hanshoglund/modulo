
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : GHC
--     
-- This Haskell module defines the Modulo description language. Typically, modules are created
-- by writing @.module@ files and using the parser defined in "Language.Modulo.Parser", or
-- the @modulo@ command-line tool. The abstract syntax tree is given below.
-- 
-- The module language is very simple. Each module consists of a name followed by
-- eventual import declarations, followed by all other declarations. Here is an example module:
--
-- > module Scl.List
-- > {
-- >   opaque Elem;
-- >   opaque List;
-- > 
-- >   nil     : () -> List;
-- >   cons    : (Elem, List) -> List;
-- >   head    : (List) -> Elem;
-- >   tail    : (List) -> List;
-- > 
-- >   empty   : (List) -> Bool;
-- >   lenght  : (List) -> Int;
-- > 
-- >   reverse : (List) -> List;
-- >   sort    : (List) -> List;
-- > }
--
-- Like C, the module language uses structural typing for pointers and functions, but
-- nominal typing for structures and unions. Thus in the following example values of type
-- @A@ and @B@ are interchangeable, but values of type @C@ and @D@ are not.
--
-- > type A = Ptr Int
-- > type B = Ptr Int
-- > type C = struct { x : Int, y : Int }
-- > type D = struct { x : Int, y : Int }
--
-- The pointer type constructor can be used with any type:
--
-- > type IntPtr = Int*
-- > type FloatPtr = Float*
--
-- The array type constructor need an explicit length:
--
-- > type IntVec = [Int x 10]
--
-- Functions are written as in Haskell, except for the parentheses enclosing the arguments.
-- Thus, higher-arity functions are easily distinguished.
--
-- > type NoCurry = (A, B) -> C
-- > type Curry   = (A) -> (B) -> C
--
-- Compound types are written in a uniform manner:
--
-- > type IntCounter = struct { a : Int, b : Int -> Int }
-- > type NumValue   = union { left : Int, right : Float }
-- > type Color      = enum { Red, Green, Blue }
--
-- The following primitive types are provided:
-- 
-- > Void Size Ptrdiff Intptr UIntptr 
-- > Char Short Int Long LongLong
-- > UChar UShort UInt ULong ULongLong
-- > Float Double LongDouble
-- > Int8 Int16 Int32 Int64 UInt8 UInt16 UInt32 UInt64
-- 
--
--
-------------------------------------------------------------------------------------

module Language.Modulo (
        -- ** Modules
        Module(..),
        ModuleOptions(..),
        ModuleName(..),
        toModuleName,
        getModuleNameList,

        -- ** Documentation
        Doc(..),
        
        -- ** Names
        Name(..),
        getName,
        
        -- ** Declarations
        Decl(..),
        getDeclName,
        Value(..),

        -- ** Types
        Type(..),
        PrimType(..),        
        RefType(..),        
        FunType(..),        
        CompType(..),        
  ) where

import Data.Ord
import Data.Default
import Data.String
import Numeric.Natural
import Foreign.C.Types      

import Data.List.NonEmpty ( NonEmpty(..) )

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty


newtype Doc = Doc { getDoc :: String }
    deriving (Eq, Ord, Show, IsString)

data ModuleOptions = ModuleOptions {
        propTransient :: Bool -- ^ If true, this module does not incur any C prefix.
    }
    deriving (Eq, Ord, Show)

instance Default ModuleOptions where
    def = ModuleOptions { propTransient = False }


-- | 
-- A module is a named container of imports and declarations.
--
-- Each module can depend on a set of other modules (translated as include directives).
-- Recursive dependencies are not allowed for now.
--
data Module 
    = Module 
      { 
      -- TODO properties:
        -- transient :: Bool (ignore last component in name for mangling)
        -- visibility :: Public | Internal
      modName    :: ModuleName,                   -- ^ Name of module
      modOptions :: ModuleOptions,                -- ^ Module options.
      modDoc     :: Doc,                          -- ^ Module documentation.
      modImports :: [(ModuleName, Maybe String)], -- ^ Imports with optional import conventions
      modDecls   :: [(Doc, Decl)]                 -- ^ List of declarations
      }
    deriving (Eq, Show)

instance Ord Module where
    compare = comparing modName
        
-- | 
-- A module name is a non-empty list of strings.
--
newtype ModuleName 
    = ModuleName 
      { 
      getModuleName :: (NonEmpty String) 
      }
    deriving (Eq, Ord)

toModuleName :: [String] -> ModuleName
toModuleName = ModuleName . NonEmpty.fromList

getModuleNameList :: ModuleName -> [String]
getModuleNameList = NonEmpty.toList . getModuleName

instance Show ModuleName where
    show (ModuleName (x :| xs)) = concat . List.intersperse "." $ x : xs


-- |
-- Name of a type, function or constant value.
--
-- Note that any Unicode string may be used as a name.
--
data Name 
    = Name  String
    | QName ModuleName String
    deriving (Eq, Ord)

getName :: Name -> String
getName (Name n)    = n
getName (QName m n) = show m ++ "." ++ n

instance Show Name where
    show = getName    


-- | 
-- An declaration maps a name to type and optionally a value.
--
data Decl 
    = TypeDecl Name (Maybe Type)         -- ^ Declares a type or opaque.
    | FunctionDecl Name FunType          -- ^ Declares a function.
    | TagDecl Type                       -- ^ Declares a struct or enum tag.
    | ConstDecl Name (Maybe Value) Type  -- ^ Declares a constant value.
    | GlobalDecl Name (Maybe Value) Type -- ^ Declares a global variable.
    deriving (Eq, Show)

getDeclName :: Decl -> Maybe Name
getDeclName (TypeDecl n t)      = Just n
getDeclName (FunctionDecl n t)  = Just n
getDeclName (TagDecl t)         = Nothing
getDeclName (ConstDecl n v t)   = Just n
getDeclName (GlobalDecl n v t)  = Just n

-- | 
-- A value is anything that can be declared as a C constant.
data Value 
    = CInt   Integer
    | CFloat Double
    | CStr   String
    | CWStr  String
    | CInitList [Value]
    deriving (Eq, Show)

-- | 
-- A type is either an alias, a primitive or a compound type.
data Type             
    = AliasType Name     -- ^ An alias, introduced by a former type declaration.
    | PrimType  PrimType
    | RefType   RefType
    | FunType   FunType
    | CompType  CompType
    deriving (Eq, Show)

-- | A primitive type.
data PrimType
    = Void | Size | Ptrdiff | Intptr | UIntptr 
    | Char  | Short  | Int  | Long  | LongLong
    | SChar
    | Bool
    | UChar | UShort | UInt | ULong | ULongLong
    | Float | Double | LongDouble
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64
    deriving (Eq, Show)
    -- StrUtf8
    -- StrUcs16Be
    -- StrUcs16Le
    -- StrUtf16Be
    -- StrUtf16Le

data RefType
    = Pointer Type          -- ^ The C pointer type @t*@.
    | Array   Type Natural  -- ^ The C array type @t[n]@.
    deriving (Eq, Show)

-- | A function type.
data FunType 
    = Function [(Maybe Name, Type)] Type -- ^ The C function type @Tn(T1, ... Tn-1)@.
    deriving (Eq, Show)

data CompType
    = Enum        (NonEmpty Name)                   -- ^ A C enum type.
    | Struct      (NonEmpty (Name, Type))           -- ^ A C struct type.
    | Union       (NonEmpty (Name, Type))           -- ^ A C union type.
    | BitField    (NonEmpty (Name, Type, Natural))  -- ^ A C bitfield type.
    deriving (Eq, Show)
    


-- TODO Decl of struct/union tags

-- TODO use Foreign.C values for Value

-- TODO Decl order by sort and name?

