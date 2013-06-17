
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
-- Renders module descriptions as Haskell 2010 foreign declarations.
--
-------------------------------------------------------------------------------------

{-                                          
    module (...) where
    data FaeString
    foreign import ccall "fae_fae_version_string"            
        c_VersionString :: IO (Ptr FaeString)
-}

module Language.Modulo.Haskell ( 
  ) where
