
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

-- |
-- Mangle an indentifier in mixed case, i.e. @[foo, bar]@ becomes @fooBar@.
mixedCase :: [String] -> String
mixedCase [] = []
mixedCase (x:xs) = mconcat $ toLowerString x : fmap toCapitalString xs

-- |
-- Mangle an indentifier in capital case, i.e. @[foo, bar]@ becomes @FooBar@.
capitalCase :: [String] -> String
capitalCase = mconcat . fmap toCapitalString

-- |
-- Mangle an indentifier using the underscore as separator, i.e. @[foo, bar]@ becomes @foo_bar@.
sepCase :: [String] -> String
sepCase = concatSep "_"
