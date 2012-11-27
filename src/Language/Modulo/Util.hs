
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
        concatSep,
        toUpperChar,
        toLowerChar,
        toUpperString,
        toLowerString,
        toCapitalString,
        divideList,
        breakList,
        withPrefix,
        withSuffix
  ) where

import qualified Data.Char   as Char
import qualified Data.Monoid as Monoid
import qualified Data.List   as List

-------------------------------------------------------------------------------------
-- String and Char
-------------------------------------------------------------------------------------

toUpperChar :: Char -> Char
toUpperChar = Char.toUpper

toLowerChar :: Char -> Char
toLowerChar = Char.toLower

toUpperString :: String -> String
toUpperString = fmap Char.toUpper

toLowerString :: String -> String
toLowerString = fmap Char.toLower

toCapitalString :: String -> String
toCapitalString [] = []
toCapitalString (x:xs) = toUpperChar x : toLowerString xs



-------------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------------

-- | Combination of @concat@ and @intersperse@.
concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . List.intersperse x

-- | Divide a list into parts of maximum length n.
divideList :: Int -> [a] -> [[a]]
divideList n xs 
    | length xs <= n = [xs]
    | otherwise      = [take n xs] ++ (divideList n $ drop n xs)

-- | Break up a list into parts of maximum length n, inserting the given list as separator.
--   Usefor for breaking up strings, as in @breakList 80 "\n" str@.
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = Monoid.mconcat . List.intersperse z . divideList n    

-- | Synonym for @(++)@
withPrefix :: [a] -> [a] -> [a]
withPrefix x = (x ++)

-- | Synonym for @flip (++)@
withSuffix :: [a] -> [a] -> [a]
withSuffix x = (++ x)
