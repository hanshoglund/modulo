
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
-- Renders module descriptions as JavaScript imports, using Node package @ffi@.
--
-- See <https://github.com/rbranson/node-ffi>
--
-------------------------------------------------------------------------------------

{-
    'fae_audio_engine_version':             ['pointer', [] ],
-}

module Language.Modulo.JavaScript ( 
  ) where
