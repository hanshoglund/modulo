
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

module Language.Modulo.C ( 
  ) where

import Data.Monoid
import Language.Modulo
import Language.C.Syntax.AST
import Language.C.Pretty
import qualified Data.List as List
import qualified Data.Char as Char

data GuardStyle = Pragma | Ifndef
data ImportStyle = Angles | Quotes


toUpper :: String -> String
toUpper = fmap Char.toUpper

toLower :: String -> String
toLower = fmap Char.toLower

toCapital :: String -> String
toCapital [] = []
toCapital (x:xs) = Char.toUpper x : toLower xs

-- fooBar
mixedCase :: [String] -> String
mixedCase [] = []
mixedCase (x:xs) = mconcat $ toLower x : fmap toCapital xs

-- FooBar
capitalCase :: [String] -> String
capitalCase = mconcat . fmap toCapital

-- foo_bar
underscoreSeparated :: [String] -> String
underscoreSeparated = mconcat . List.intersperse "_" . fmap toLower

prefixedBy x = (x ++)
suffixedBy x = (++ x)


      
data CStyle =
    CStyle {
        guardStyle :: GuardStyle,
        importDirective :: String,
        importStyle :: ImportStyle,
        guardMangler :: [String] -> String,
        prefixMangler :: [String] -> String,
        
        implStructNameMangler :: [String] -> String,
        realStructNameMangler :: [String] -> String,
        unionNameMangler :: [String] -> String,
        enumNameMangler :: [String] -> String,

        structFieldMangler :: [String] -> String,
        unionFieldMangler :: [String] -> String,
        enumFieldMangler :: [String] -> String,
        
        globalNameMangler :: [String] -> String,
        constNameMangler :: [String] -> String,
        functionNameMangler :: [String] -> String
    }

-- | Style similar to Haskell/Java
haskellStyle :: CStyle
haskellStyle = CStyle
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    capitalCase

    capitalCase
    capitalCase
    capitalCase
    capitalCase

    mixedCase
    mixedCase
    mixedCase
    
    mixedCase
    mixedCase
    mixedCase

-- | Style user Apple Frameworks    
appleStyle :: CStyle
appleStyle = CStyle 
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    capitalCase
    
    capitalCase
    capitalCase
    capitalCase
    capitalCase

    (prefixedBy "m" . capitalCase)
    (prefixedBy "m" . capitalCase)
    (prefixedBy "m" . capitalCase)
    
    (prefixedBy "g" . capitalCase)
    (prefixedBy "k" . capitalCase)
    capitalCase

-- | Style used in GTK and many open source applications    
gtkStyle :: CStyle
gtkStyle = CStyle 
    Ifndef "include" Angles
    (prefixedBy "_" . underscoreSeparated . fmap toUpper)
    underscoreSeparated
    
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (suffixedBy "_t" . underscoreSeparated)
    (underscoreSeparated)
    (underscoreSeparated)
    (underscoreSeparated)

    underscoreSeparated
    underscoreSeparated
    underscoreSeparated


convertModule :: CStyle -> Module -> CTranslUnit
convertModule style mod = undefined


printModule :: CStyle -> Module -> String
printModule style = show . pretty . convertModule style
