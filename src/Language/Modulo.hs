
{-# LANGUAGE DisambiguateRecordFields, TypeFamilies,
    StandaloneDeriving, DeriveFunctor, DeriveFoldable, GeneralizedNewtypeDeriving #-}

-------------------------------------------------------------------------------------
-- |
-- Module      : Language.Modulo
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
        getModuleNameParts,
        Id,
        ModuleDecl(..),
        Value(..),
        Type(..),
        PrimType(..),        
  ) where
      
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

data Module 
    = Module { modName :: ModuleName, modImports :: [ModuleName], modDecls :: [ModuleDecl] }
    deriving (Eq, Show)
        
newtype ModuleName 
    = ModuleName { getModuleName :: (NonEmpty String) }
    deriving (Eq, Ord)

getModuleNameParts :: ModuleName -> [String]
getModuleNameParts = NonEmpty.toList . getModuleName

instance Show ModuleName where
    show (ModuleName (x :| xs)) = concat . List.intersperse "." $ x : xs

type Id = String

data ModuleDecl 
    = TypeDecl Id Type
    | ConstDecl Id (Maybe Value) Type
    | GlobalDecl Id (Maybe Value) Type
    | FunctionDecl Id Type
    deriving (Eq, Show)

type Value 
    = Int -- TODO use Foreign.C values here

data Type 
    = PrimType    PrimType
    | Pointer     Type
    | DynArray    Type
    | Array       Type Int
    | Function    [Type] Type
    | Struct      (NonEmpty (Id, Type))
    | Union       (NonEmpty (Id, Type))
    -- | TagUnion    (NonEmpty (Id, Type))
    | Enum        (NonEmpty Id)
    | Alias       Type
    | Ref         Id
    deriving (Eq, Show)

data PrimType
    = Void 
    | Char | Short | Int | Long | LongLong
    | UnsignedChar | UnsignedShort | UnsignedInt | UnsignedLong | UnsignedLongLong
    | Float | Double | LongDouble
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64
    deriving (Eq, Show)


