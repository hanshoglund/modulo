
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
import Control.Monad
import System.Directory
import System.Process
import Text.Pandoc.Options
import Text.Pandoc.Templates
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
  Header 2 ("", [], [("id",show $ modName mod)]) [Str (show $ modName mod)]
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
  CodeBlock css $ unname $ getDeclName decl
  ,
  Para $ return $ Str $ getDoc $ doc


  ]
  where
    -- unname = maybe "" (show . C.translFun def)
    -- unname = maybe "" getShortName
    unname = if isTypeDecl decl 
      then ("(defclass " ++) . (++ " ())") . maybe "" (Lisp.convertName def) 
      else (\x -> "(defun " ++ x ++ " (" ++ List.intercalate " " (argNames decl) ++ "))")    . maybe "" (Lisp.convertName def)
      where
        isTypeDecl (TypeDecl _ _) = True
        isTypeDecl _              = False

        argNames (FunctionDecl _ ft) = argNames2 ft
        argNames2 (Function as r) = fmap showT as ++ [showT r]
        showT = show . Lisp.convertType def

    css = (".codeName", [], [("", "")])
    
    getShortName (QName _ n) = n



  
-- allFilesMatching :: FilePath -> (FilePath -> Bool) -> IO [FilePath]







main = documentFiles ["/Users/hans/audio/modules"] "/Users/hans/audio/modules"
-- main = documentFile ["/Users/hans/audio/modules"] "/Users/hans/audio/modules/Fa/Signal.module"

document :: [ModulePath] -> String -> IO Pandoc
document mpaths str = do
  mod <- unsafeRename mpaths . unsafeParse $ str
  putStr $ "Documenting " ++ show (modName mod) ++ "\n"
  return $ renderModulePandoc mod

documentFiles :: [ModulePath] -> FilePath -> IO ()
documentFiles mpaths path = do
  paths <- listFilesMatching path (List.isSuffixOf ".module")
  pandocs <- mapM (\path -> document mpaths =<< readFile path) paths
  strs <- toHtmlStr $ mconcat pandocs
  writeFile "test.html" $ strs
  return ()
    where
      cssBody = mempty
        <> "body > pre {                                      "
        <> "    padding:                20px;                 "
        <> "    margin-top:             15px;                 "
        <> "    margin-bottom:          15px;                 "
        <> "    background:             #faffff;              "
        <> "    border-radius:          12px;                 "
        <> "    border:                 1px solid LightGrey;  "
        <> "    box-shadow:             3px 3px 5px #eee;     "
        <> "    overflow:               auto;                 "
        <> "}"
        <> ""
        <> ""
        <> ""
        <> ""
        <> ""
        <> ""
        <> ""
        <> ""
      
      toHtmlStr :: Pandoc -> IO String
      toHtmlStr str = do
        Right templ <- getDefaultTemplate Nothing "html"
        return $ flip writeHtmlString str $ def {
          writerTemplate = templ,
          writerStandalone = True,
          writerTableOfContents = True,
          writerVariables = [("highlighting-css", cssBody)] }

documentFile :: [ModulePath] -> FilePath -> IO ()
documentFile mpaths path = do                 
  pd <- document mpaths =<< readFile path
  writeFile "test.html" $ writeHtmlString def pd                 




unsafeRename :: [ModulePath] -> Module -> IO Module
unsafeRename paths m = do
    deps <- loadDependencies (withStdModulePaths paths) m
    return $ rename deps m
    
unsafeParse :: String -> Module
unsafeParse s = case (parse s) of
    Left e -> error $ "Parse error: " ++ show e
    Right m -> m
    

listFilesMatching :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
listFilesMatching path pred = fmap (filter pred) $ listFilesR path

listFilesR :: FilePath -> IO [FilePath]
listFilesR = listFilesR' . (<> "/")

listFilesR' path = let
    isDODD :: String -> Bool
    isDODD f = not $ (List.isSuffixOf "/." f) || (List.isSuffixOf "/.." f)
    -- isDODD _ = True

    listDirs :: [FilePath] -> IO [FilePath]
    listDirs = filterM doesDirectoryExist . fmap (<> "/")

    listFiles :: [FilePath] -> IO [FilePath]
    listFiles = filterM doesFileExist

    joinFN :: String -> String -> FilePath
    joinFN p1 p2 = mconcat [p1, p2]

    in do
        allfiles <- getDirectoryContents path
        no_dots <- filterM (return . isDODD) (map (joinFN path) allfiles)
        dirs <- listDirs no_dots
        subdirfiles <- (mapM (listFilesR'{- . (<> "/")-}) dirs >>= return . concat)
        files <- listFiles no_dots
        return $ files ++ subdirfiles  
