
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
--
-------------------------------------------------------------------------------------

module Language.Modulo.Rename (
        rename
  ) where

import Control.Exception
import Data.Maybe (catMaybes)

import Language.Modulo
import Language.Modulo.Parse
import Language.Modulo.Util

import qualified Data.List.NonEmpty as NonEmpty

-- |
-- Rewrite all unqualified names as qualified names.
--
-- This function is partial with the following invariants:
--
-- * Received module has no QName constructors
--
-- * Returned module has no Name constructors
--
rename :: [Module] -> Module -> Module
rename deps mod@(Module n is ds) = Module n is (map renameDecl ds) 
    where
        renameDecl (TypeDecl n t)      = TypeDecl (qualify mod n) (fmap renameType t)
        renameDecl (FunctionDecl n t)  = FunctionDecl (qualify mod n) (renameFunType t)
        renameDecl (TagDecl t)         = TagDecl (renameType t)
        renameDecl (ConstDecl n v t)   = ConstDecl (qualify mod n) v (renameType t)
        renameDecl (GlobalDecl n v t)  = GlobalDecl (qualify mod n) v (renameType t)

        renameType (PrimType t)  = PrimType t
        renameType (AliasType n) = AliasType $ resolveName (mod : deps) n
        renameType (RefType t)   = RefType   $ renameRefType t
        renameType (FunType t)   = FunType   $ renameFunType t
        renameType (CompType t)  = CompType  $ renameCompType t

        renameRefType :: RefType -> RefType
        renameRefType (Pointer t) = Pointer (renameType t)
        renameRefType (Array t j) = Array (renameType t) j
        
        renameFunType :: FunType -> FunType
        renameFunType (Function as r) = Function (fmap renameType as) (renameType r)
        
        renameCompType :: CompType -> CompType
        renameCompType (Enum ns)     = Enum ns
        renameCompType (Struct ns)   = Struct $ fmap (\(n,t) -> (n, renameType t)) ns
        renameCompType (Union ns)    = Union  $ fmap (\(n,t) -> (n, renameType t)) ns
        renameCompType (BitField ns) = notSupported "Bit-fields"

qualify :: Module -> Name -> Name
qualify m (Name n)    = QName (modName m) n
qualify _ (QName _ _) = error "Name already qualified"

resolveName :: [Module] -> Name -> Name
resolveName ms (QName m n) = QName m n
resolveName ms n@(Name n') = case findName ms n of
    Nothing -> error $ "Could not find: " ++ show n'
    Just m  -> QName m n'
        
-- | 
-- Find the first module in which the given unqualified name is declared
--
findName :: [Module] -> Name -> Maybe ModuleName
findName []     n = Nothing
findName (m:ms) n
    | n `elem` mNs   = Just $ modName m
    | otherwise      = findName ms n
    where
        mNs = catMaybes . map getDeclName . modDecls $ m 











notSupported x = error $ "Not supported yet: " ++ x
