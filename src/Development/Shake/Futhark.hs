module Development.Shake.Futhark ( getFutDeps
                                 , getAllFutDeps
                                 , needFut
                                 ) where

import           Control.Monad             ((<=<))
import           Data.Containers.ListUtils (nubOrd)
import           Data.Foldable             (traverse_)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Development.Shake         (Action, need, traced)
import           Language.Futhark.Parser   (SyntaxError (..), parseFuthark)
import           Language.Futhark.Syntax   (DecBase (..), ModBindBase (ModBind), ModExpBase (..), ProgBase (Prog), locStr)
import           System.Directory          (canonicalizePath, makeRelativeToCurrentDirectory)
import           System.FilePath           (takeDirectory, (<.>), (</>))

-- | @'need'@ a file and all its dependencies
needFut :: [FilePath] -> Action ()
needFut fps = do
    next <- traced "getFutDeps" $ traverse getFutDeps fps
    need (concat next)
    traverse_ needFut next

getFutDeps :: FilePath -> IO [FilePath]
getFutDeps fp = traverse canonicalizeRelative =<< do
    contents <- TIO.readFile fp
    let dirFile = takeDirectory fp
        parsed = either (error.showErr) id $ parseFuthark fp contents
    pure ((dirFile </>) . (<.> "fut") <$> extractFromProgBase parsed)
    where showErr (SyntaxError l str) = locStr l ++ ": " ++ T.unpack str

-- | Get all transitive dependencies
getAllFutDeps :: FilePath -> IO [FilePath]
getAllFutDeps fp = do
    deps <- getFutDeps fp
    level <- traverse getAllFutDeps deps
    let next = nubOrd (concat (deps : level))
    pure $ if null level then deps else next

canonicalizeRelative :: FilePath -> IO FilePath
canonicalizeRelative = makeRelativeToCurrentDirectory <=< canonicalizePath

extractFromProgBase :: ProgBase f vn -> [FilePath]
extractFromProgBase (Prog _ ds) = concatMap extractFromDecBase ds

extractFromDecBase :: DecBase f vn -> [FilePath]
extractFromDecBase (ImportDec fp _ _)             = [fp]
extractFromDecBase (LocalDec d _)                 = extractFromDecBase d
extractFromDecBase (OpenDec d _)                  = extractFromModExpBase d
extractFromDecBase (ModDec (ModBind _ _ _ m _ _)) = extractFromModExpBase m
extractFromDecBase ValDec{}                       = []
extractFromDecBase TypeDec{}                      = []
extractFromDecBase ModTypeDec{}                   = []

extractFromModExpBase :: ModExpBase f vn -> [FilePath]
extractFromModExpBase (ModParens m _)       = extractFromModExpBase m
extractFromModExpBase (ModImport fp _ _)    = [fp]
extractFromModExpBase (ModDecs ds _)        = concatMap extractFromDecBase ds
extractFromModExpBase (ModApply m m' _ _ _) = concatMap extractFromModExpBase [m, m']
extractFromModExpBase (ModAscript m _ _ _)  = extractFromModExpBase m
extractFromModExpBase (ModLambda _ _ m _)   = extractFromModExpBase m
extractFromModExpBase ModVar{}              = []
