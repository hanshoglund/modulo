
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
        ModuleDecl(..),
        Value(..),
        Type(..),
        PrimType(..),
        
        testModule,
  ) where
      
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

data Module 
    = Module ModuleName [ModuleName] [ModuleDecl] -- name imports decls
    deriving (Eq, Show)
        
newtype ModuleName 
    = ModuleName (NonEmpty String)
    deriving (Eq, Ord)

instance Show ModuleName where
    show (ModuleName (x :| xs)) = concat . List.intersperse "." $ x : xs

data ModuleDecl 
    = TypeDecl String Type
    | GlobalDecl String (Maybe Value) Type
    | ConstDecl String (Maybe Value) Type
    | FunctionDecl String Type
    deriving (Eq, Show)

type Value 
    = Int -- TODO use Foreign.C values here

data Type 
    = PrimType PrimType
    | Pointer     Type
    | DynArray    Type
    | Array       Type Int
    | Function    [Type] Type
    | Struct      (NonEmpty (String, Type))
    | Union       (NonEmpty (String, Type))
    -- | TagUnion    (NonEmpty (String, Type))
    | Enum        (NonEmpty String)
    | Alias       Type
    | Ref         String
    deriving (Eq, Show)

data PrimType
    = Void 
    | Char | Short | Int | Long | LongLong
    | UnsignedChar | UnsignedShort | UnsignedInt | UnsignedLong | UnsignedLongLong
    | Float | Double | LongDouble
    | Int8 | Int16 | Int32 | Int64 | UInt8 | UInt16 | UInt32 | UInt64
    deriving (Eq, Show)


-- module Foo
--     module Bar
--         import X.Y.Z.Baz
--         
--         struct Note { pitch : Pitch }
--         enum Pitch { C, D, E }
--         
--         foo = 5 : Int
-- 
--         foo : Note -> Note
--         bar : Pitch -> Pitch
testModule = 
    Module (ModuleName $ NonEmpty.fromList ["Foo", "Bar"]) 
        [ModuleName $ NonEmpty.fromList ["X", "Y", "Z", "Bar"]]
        [   
            TypeDecl "Note" (Struct $ NonEmpty.fromList [("pitch", Ref "Pitch")]),
            TypeDecl "Pitch" (Enum $ NonEmpty.fromList ["C", "D", "E"]),
            GlobalDecl "foo" (Just 5) (PrimType Int)
        ]
