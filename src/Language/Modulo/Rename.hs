
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
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List as List

import Language.Modulo
import Language.Modulo.Parse
import Language.Modulo.Util
import Language.Modulo.Util.Unmangle (unmangle)

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
rename deps mod@(Module doc n is ds) = Module doc n is (map (fmap renameDecl) ds) 
    where
        renameDecl (TypeDecl n t)      = TypeDecl (simplify $ qualify mod n) (fmap renameType t)
        renameDecl (FunctionDecl n t)  = FunctionDecl (simplify $ qualify mod n) (renameFunType t)
        renameDecl (TagDecl t)         = TagDecl (renameType t)
        renameDecl (ConstDecl n v t)   = ConstDecl (simplify $ qualify mod n) v (renameType t)
        renameDecl (GlobalDecl n v t)  = GlobalDecl (simplify $ qualify mod n) v (renameType t)

        renameType (PrimType t)  = PrimType t
        renameType (AliasType n) = AliasType $ simplify $ resolveName mod (mod : deps) n
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

resolveName :: Module -> [Module] -> Name -> Name
resolveName errorMsgMod deps (QName m n) = QName m n
resolveName errorMsgMod deps n@(Name n') = case findName deps n of
    Nothing -> error $ "Could not find '" ++ show n' ++ "' in module " ++ show (modName errorMsgMod)
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
        mNs = catMaybes . map (getDeclName . snd) . modDecls $ m 


-- If the given name is a suffix of the module name, simplify
simplify :: Name -> Name
simplify (Name n)    = Name n
simplify (QName m n) = QName (simp m n) n
    where
        simp mn@(ModuleName (m :| ms)) n
            | unmangle n `isSuffixOf` concatMap unmangle ms 
                = ModuleName (m :| concat (dropNestEnd (length $ unmangle n) (map unmangle ms)))
            | otherwise     = mn

        x `lastOf` [] = False
        x `lastOf` xs = x == last xs


dropNestEnd :: Int -> [[a]] -> [[a]]
dropNestEnd n = reverseDeep . dropNest n . reverseDeep

reverseDeep :: [[a]] -> [[a]]
reverseDeep = reverse . map reverse
        
dropNest :: Int -> [[a]] -> [[a]]
dropNest _ []  = []
dropNest 0 xss = xss
dropNest n (xs:xss) = drop n xs : dropNest m xss
    where
        m = n - length xs `max` 0
        






notSupported x = error $ "Not supported yet: " ++ x
