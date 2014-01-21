
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
-- Renders module descriptions (and documentation) as plain Markdown.
--
-------------------------------------------------------------------------------------

module Language.Modulo.Lisp (
        -- ** Styles
        MarkdownStyle(..),
        stdMarkdownStyle,
        -- ** Rendering
        printModuleMarkdown,
        renderModuleMarkdown,
  ) where

import Data.Default
import Data.Semigroup
import Data.Char (chr)
import Data.Text (pack)
import Data.AttoLisp

import Language.Modulo.C
import Language.Modulo.Util
import Language.Modulo.Util.Unmangle
import Language.Modulo

import qualified Data.List as List