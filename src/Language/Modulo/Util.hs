
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
        -- ** String and Char stuff
        toUpperChar,
        toLowerChar,
        toUpperString,
        toLowerString,
        toCapitalString,

        -- ** List stuff
        withPrefix,
        withSuffix,
        concatSep,
        divideList,
        breakList
  ) where

import qualified Data.Char   as Char
import qualified Data.Monoid as Monoid
import qualified Data.List   as List

-------------------------------------------------------------------------------------
-- String and Char
-------------------------------------------------------------------------------------

-- | 
-- Synonym for @Char.toUpper@
toUpperChar :: Char -> Char
toUpperChar = Char.toUpper

-- | 
-- Synonym for @Char.toLower@
toLowerChar :: Char -> Char
toLowerChar = Char.toLower

-- | 
-- Synonym for @fmap Char.toUpper@
toUpperString :: String -> String
toUpperString = fmap Char.toUpper

-- | 
-- Synonym for @fmap Char.toLower@
toLowerString :: String -> String
toLowerString = fmap Char.toLower

-- | 
-- Convert a string to use upper case for the leading letter and lower case for 
-- remaining letters.
toCapitalString :: String -> String
toCapitalString [] = []
toCapitalString (x:xs) = toUpperChar x : toLowerString xs



-------------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------------

-- | 
-- Synonym for @(++)@
withPrefix :: [a] -> [a] -> [a]
withPrefix x = (x ++)

-- | 
-- Synonym for @flip (++)@
withSuffix :: [a] -> [a] -> [a]
withSuffix x = (++ x)

-- | 
-- Combination of @concat@ and @intersperse@.
concatSep :: [a] -> [[a]] -> [a]
concatSep x = List.concat . List.intersperse x

-- | 
-- Divide a list into parts of maximum length n.
divideList :: Int -> [a] -> [[a]]
divideList n xs 
    | length xs <= n = [xs]
    | otherwise      = [take n xs] ++ (divideList n $ drop n xs)

-- | 
-- Break up a list into parts of maximum length n, inserting the given list as separator.
-- Useful for breaking up strings, as in @breakList 80 "\n" str@.
breakList :: Int -> [a] -> [a] -> [a]
breakList n z = Monoid.mconcat . List.intersperse z . divideList n    

