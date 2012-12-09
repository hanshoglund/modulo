
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
        resolve
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
resolve :: [Module] -> Module -> Module
resolve deps mod@(Module n is ds) = Module n is (map renameDecl ds) 
    where
        renameDecl (TypeDecl n t)      = TypeDecl n (fmap renameType t)
        renameDecl (FunctionDecl n t)  = FunctionDecl n (renameFunType t)
        renameDecl (TagDecl t)         = TagDecl (renameType t)
        renameDecl (ConstDecl n v t)   = ConstDecl n v (renameType t)
        renameDecl (GlobalDecl n v t)  = GlobalDecl n v (renameType t)

        renameType (PrimType t)  = PrimType t
        renameType (AliasType n) = AliasType $ rename (mod : deps) n
        renameType (RefType t)   = RefType   $ renameRefType t
        renameType (FunType t)   = FunType   $ renameFunType t
        renameType (CompType t)  = CompType  $ renameCompType t

        renameRefType :: RefType -> RefType
        renameRefType (Pointer t) = Pointer (renameType t)
        renameRefType (Array t n) = Array (renameType t) n
        
        renameFunType :: FunType -> FunType
        renameFunType (Function as r) = Function (fmap renameType as) (renameType r)
        
        renameCompType :: CompType -> CompType
        renameCompType (Enum ns)     = Enum ns
        renameCompType (Struct ns)   = Struct $ fmap (\(n,t) -> (n, renameType t)) ns
        renameCompType (Union ns)    = Union  $ fmap (\(n,t) -> (n, renameType t)) ns
        renameCompType (BitField ns) = notSupported "Bit-fields"

rename :: [Module] -> Name -> Name
rename ms (QName m n) = QName m n
rename ms n@(Name n') = case resolveName ms n of
    Nothing -> Name n'
    Just m  -> QName m n'
        
-- | Find the first module in which the given unqualified name is declared
resolveName :: [Module] -> Name -> Maybe ModuleName
resolveName []     n = Nothing
resolveName (m:ms) n
    | n `elem` mNs   = Just $ modName m
    | otherwise      = resolveName ms n
    where
        mNs = catMaybes . map getDeclName . modDecls $ m 




notSupported x = error $ "Not supported yet: " ++ x
