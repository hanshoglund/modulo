
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
-- Manglers for various kinds of identifiers.
--
-------------------------------------------------------------------------------------

module Language.Modulo.Util.Mangle (
        mixedCase,
        capitalCase,
        sepCase
  ) where

import Data.Semigroup
import Data.Semigroup

import Language.Modulo.Util

-- fooBar
mixedCase :: [String] -> String
mixedCase [] = []
mixedCase (x:xs) = mconcat $ toLowerString x : fmap toCapitalString xs

-- FooBar
capitalCase :: [String] -> String
capitalCase = mconcat . fmap toCapitalString

-- foo_bar
sepCase :: [String] -> String
sepCase = concatSep "_"
