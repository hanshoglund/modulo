
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
-- Unmanglers for various kinds of identifiers.
--
-------------------------------------------------------------------------------------

module Language.Modulo.Util.Unmangle (
        unmangle,
        uncase,
        unsep
  ) where


import Data.Semigroup
import Data.Semigroup
import Language.Modulo.Util

import qualified Data.Char as Char
import qualified Data.List as List

-- |
-- Unmangle an indentifier in mixed or separarated case.
unmangle :: String -> [String]
unmangle "" = []
unmangle a = mconcat . fmap uncase $ unsep a

-- |
-- Unmangle an indentifier in mixed case, i.e. @fooBar@ becomes @[foo, bar]@.
uncase :: String -> [String]
uncase ""     = []
uncase (c:cs) = case n of
    Nothing -> [c:cs]
    Just n  -> (c : take n cs) : uncase (drop n cs)
    where
        n = List.findIndex (Char.isUpper) cs
        
-- |
-- Unmangle an indentifier using the underscore as separator, i.e. @foo_bar@ becomes @[foo, bar]@.
--
-- > unsep (sep a) = a, iff a /= []
--
unsep :: String -> [String]
unsep [] = []
unsep cs = unsep' cs

unsep' cs = case n of
    Nothing -> [cs]
    Just n  -> take n cs : unsep' (drop (n+1) cs)
    where                
        n = List.findIndex isSep cs        
        isSep '_' = True
        isSep _   = False