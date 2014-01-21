
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
-- Renders module descriptions (and documentation) as plain Pandoc.
--
-------------------------------------------------------------------------------------

module Language.Modulo.Pandoc (
        -- ** Styles
        PandocStyle(..),
        stdPandocStyle,
        -- ** Rendering
        -- printModulePandoc,
        renderModulePandoc,
  ) where

import Data.Default
import Data.Semigroup
import Data.Char (chr)
import Data.Text (pack)

import Language.Modulo.C
import Language.Modulo.Util
import Language.Modulo.Util.Unmangle
import Language.Modulo
import qualified Language.Modulo.C as C
import qualified Language.Modulo.Haskell as Haskell
import qualified Language.Modulo.Lisp as Lisp

-- DEBUG
import System.Process
import Text.Pandoc.Options
import Language.Modulo.Load
import Language.Modulo.Parse
import Language.Modulo.Rename
-- DEBUG

import qualified Data.List as List
import Text.Pandoc.Definition
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.LaTeX

data PandocStyle = PandocStyle
  deriving (Eq, Ord, Show)

stdMeta = Meta [] [] []
blockToPandoc = Pandoc stdMeta . return
blocksToPandoc = Pandoc stdMeta
instance Semigroup Pandoc where
  (<>) = mappend
instance Monoid Pandoc where
  mempty = Pandoc stdMeta mempty
  Pandoc m1 bs1 `mappend` Pandoc m2 bs2 = Pandoc (m1 {-First-}) (bs1 <> bs2)

stdPandocStyle = PandocStyle

renderModulePandoc :: Module -> Pandoc
renderModulePandoc = renderModulePandocStyle stdPandocStyle


renderModulePandocStyle :: PandocStyle -> Module -> Pandoc
renderModulePandocStyle st mod = Pandoc stdMeta [
  Header 1 nullAttr [Str (show $ modName mod)]
  -- CodeBlock nullAttr "import X.X.X",
  -- CodeBlock nullAttr "foo : Ptr -> Ptr"
  ] <> is <> ds
  where
    is = mconcat $ fmap (uncurry $ convertImport st) $ modImports mod
    ds = mconcat $ fmap (uncurry $ convertDocDecl st) $ modDecls mod

-- TODO link
convertImport :: PandocStyle -> ModuleName -> Maybe String -> Pandoc
convertImport st name conv = blockToPandoc $ CodeBlock nullAttr $ "import " ++ show name

convertDocDecl :: PandocStyle -> Doc -> Decl -> Pandoc
convertDocDecl st doc decl = blocksToPandoc [
  CodeBlock css $ unname $ getDeclName decl,
  Para $ return $ Str $ getDoc $ doc
  ]
  where
    unname = maybe "" (Lisp.convertName def)
    -- unname = maybe "" getShortName

    css = (".codeName", [], [("style", "background: #efeeee")])
    
    getShortName (QName _ n) = n


main = do                 
  str <-  readFile "/Users/hans/audio/modules/Fa/Signal.module"
  let m  = unsafeParse str
  mr <-    unsafeRename ["/Users/hans/audio/modules"] m
  let pd = renderModulePandoc mr
  -- print pd
  writeFile "test.html" $ writeHtmlString def pd                 
  
  template <- getDefaultTemplateSource Nothing "latex"
  writeFile "test.tex" $ writeLaTeX (def {writerStandalone = True}) pd
  -- system "pdflatex test.tex"

  where
    unsafeRename :: [ModulePath] -> Module -> IO Module
    unsafeRename paths m = do
        deps <- loadDependencies (withStdModulePaths paths) m
        return $ rename deps m
        
    unsafeParse :: String -> Module
    unsafeParse s = case (parse s) of
        Left e -> error $ "Parse error: " ++ show e
        Right m -> m
    