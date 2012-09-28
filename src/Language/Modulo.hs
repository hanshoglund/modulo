
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglunds.se
-- Stability   : experimental
-- Portability : GHC
--
-------------------------------------------------------------------------------------

module Language.Modulo (
        Module(..),
        ModuleName(..),
        Declaration(..),
        Name,
        Value(..),
        Type(..),
        PrimType(..),        
        FunctionType(..),        
  ) where

import Data.Ord
import Numeric.Natural      
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

-- | A module is a named container of imports and declarations.
data Module 
    = Module { modName :: ModuleName, modImports :: [ModuleName], modDeclarations :: [Declaration] }
    deriving (Eq, Show)

instance Ord Module where
    compare = comparing modName
        
-- | A module name is a non-empty list of strings.
newtype ModuleName 
    = ModuleName { moduleName :: (NonEmpty String) }
    deriving (Eq, Ord)

instance Show ModuleName where
    show (ModuleName (x :| xs)) = concat . List.intersperse "." $ x : xs

-- | A name is a string.
type Name = String

-- | An declaration maps a name to type and (optionally) a value.

-- TODO Require ValueType (factor out everything but FunctionType)
--  for ConstDecl and GlobalDecl
data Declaration 
    -- | Declares a type using typedef
    = TypeDecl Name Type
    -- | Declares a function.
    | FunctionDecl Name FunctionType
    -- | Declares a constant value
    | ConstDecl Name (Maybe Value) Type
    -- | Declares a global variable
    | GlobalDecl Name (Maybe Value) Type
    deriving (Eq, Show)

-- | A value is anything that can be declared as a C constant.
type Value 
    = Int -- TODO use Foreign.C values here

-- | A type is either an alias, a primitive or a compound type.

-- TODO Distinguishing between tag and typedef for structs and unions.
--      I.e. which of these to declare?
--          typedef struct _x {...} x;
--          typedef struct {...} x;
--          struct _x {...};

data Type             
    -- | An alias type, introduced by a type declaration.
    = Alias       Name
    -- | An primitive type.
    | Prim        PrimType
    -- | The C pointer type constructor @T*@.
    | Pointer     Type
    -- | The C dynamic array constructor @T[]@.
    | DynArray    Type
    -- | The C static array constructor @T[n]@.
    | Array       Type Natural
    -- | See `FunctionType`.
    | Function    FunctionType
    -- | A C enumeration type.
    | Enum        (NonEmpty Name)
    -- | A C struct type.
    | Struct      (NonEmpty (Name, Type))
    -- | A C union type.
    | Union       (NonEmpty (Name, Type))
    -- TagUnion    (NonEmpty (Name, Type))
    -- | A C bitfield type.
    | BitField    (NonEmpty (Name, Type, Natural))
    deriving (Eq, Show)

-- | A function type, factored out as a separate type so that situations
--   where function types are required can be specified exactly.
data FunctionType = FunctionType [Type] Type -- ^ The C function constructor @Tn(T1, ... Tn-1)@.
    deriving (Eq, Show)

-- | A primitive type.
data PrimType
    = Void | Size | Ptrdiff | Intptr | UIntptr 
    | Char | Short | Int | Long | LongLong
    | UChar | UShort | UInt | ULong | ULongLong
    | Float | Double | LongDouble
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64
    deriving (Eq, Show)


