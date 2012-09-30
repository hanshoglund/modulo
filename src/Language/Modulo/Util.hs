
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
-------------------------------------------------------------------------------------

module Language.Modulo.Util (
    divideList,
    breakList
  ) where

import qualified Data.List
import qualified Data.Monoid

-- | Divide a list into parts of maximum length n.
divideList :: Int -> [a] -> [[a]]
divideList n xs 
    | length xs <= n = [xs]
    | otherwise      = [take n xs] ++ (divideList n $ drop n xs)

-- | Break up a list into parts of maximum length n, inserting the given list as separator.
--   Usefor for breaking up strings, as in @breakList 80 "\n" str@.
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = Data.Monoid.mconcat . Data.List.intersperse z . divideList n    
