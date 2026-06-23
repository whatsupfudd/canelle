module Cannelle.Elm.Parser ( scan ) where

import Control.Applicative ( many, optional, asum, (<|>) )

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types ( NodeEntry(..), SegmentPos )

import Cannelle.Elm.AST
import qualified Cannelle.Elm.Parser.Module as ModP
import qualified Cannelle.Elm.Parser.Import as ImpP
import qualified Cannelle.Elm.Parser.Decl as DeclP
import Cannelle.Elm.Parser.Types (ParserP)
import Cannelle.Elm.Parser.Utils (spanNE, syntheticSpan)


data TopItem =
    TopModule ModuleDecl
  | TopImport ImportDecl
  | TopDecl Decl
  | TopDiags [Diagnostic]


scan :: [NodeEntry] -> Either E.TError Context
scan nodes =
  let
    mainScanner = scanCtx <* Sc.pEof
    result = Sc.doScan mainScanner nodes
  in
    case result of
      Left err ->
        Left $ E.TError $ E.showScanErrorBundle err

      Right (ctx, demands) ->
        Right ctx { demandsCtx = demands }


scanCtx :: ParserP Context
scanCtx = do
  items <- many scanTop

  let
    modules = [ m | TopModule m <- items ]
    imports = [ i | TopImport i <- items ]
    decls = [ d | TopDecl d <- items ]
    diags   = concat [ ds | TopDiags ds <- items ]
    moduleRez = case modules of
      [] -> Nothing
      m : _ -> Just m
    duplicateModuleDiags = case modules of
      [] -> []
      [_] -> []
      _ : extras -> fmap (\m -> DuplicateModuleDiag (spanMod m)) extras

  pure emptyCtx {
      moduleCtx = moduleRez
    , importsCtx = V.fromList imports
    , declsCtx = V.fromList decls
    -- , commentsCtx = V.fromList commentsa
    -- , demandsCtx = V.empty
    , diagsCtx = V.fromList (diags <> duplicateModuleDiags)
    }


scanTop :: ParserP TopItem
scanTop = asum [ 
    TopModule <$> ModP.scanModule
  , TopImport <$> ImpP.scanImport
  , TopDecl <$> DeclP.scanDecl
  , scanSkip
  ]


scanSkip :: ParserP TopItem
scanSkip = do
  -- Remaining nodes should be skipped with something like a: ne <- Sc.anySingle
  tmpSkipAny


{- Notes:
If `Sc.anySingle` does not currently exist, add one of these options:

1. Prefer adding a small generic `anySingle` helper to `Cannelle.TreeSitter.Scanner` later.
2. For PR2, use a local catch-all scanner based on the existing lower-level `token` primitive.
3. Temporarily skip unknown nodes by adding explicit stubs for known later nodes: `type_annotation`, `value_declaration`, `type_alias_declaration`, `type_declaration`, `line_comment`, `block_comment`.

To keep PR2 small, option 3 is acceptable. We'll implement a 'tmpSkipAny' to work on the explicit stubs.

-}

tmpSkipAny :: ParserP TopItem
tmpSkipAny = asum [
  tmpSkipTypeAnnotation
  , tmpSkipValueDeclaration
  , tmpSkipTypeAliasDeclaration
  , tmpSkipTypeDeclaration
  , tmpSkipLineComment
  , tmpSkipBlockComment
  ]

tmpSkipTypeAnnotation :: ParserP TopItem
tmpSkipTypeAnnotation = do
  ne <- Sc.single "type_annotation"
  pure $ TopDiags [ UnhandledNodeDiag "type_annotation" (spanNE ne) ]

tmpSkipValueDeclaration :: ParserP TopItem
tmpSkipValueDeclaration = do
  ne <- Sc.single "value_declaration"
  pure $ TopDiags [ UnhandledNodeDiag "value_declaration" (spanNE ne) ]

tmpSkipTypeAliasDeclaration :: ParserP TopItem
tmpSkipTypeAliasDeclaration = do
  ne <- Sc.single "type_alias_declaration"
  pure $ TopDiags [ UnhandledNodeDiag "type_alias_declaration" (spanNE ne) ]

tmpSkipTypeDeclaration :: ParserP TopItem
tmpSkipTypeDeclaration = do
  ne <- Sc.single "type_declaration"
  pure $ TopDiags [ UnhandledNodeDiag "type_declaration" (spanNE ne) ]

tmpSkipLineComment :: ParserP TopItem
tmpSkipLineComment = do
  ne <- Sc.single "line_comment"
  pure $ TopDiags [ UnhandledNodeDiag "line_comment" (spanNE ne) ]

tmpSkipBlockComment :: ParserP TopItem
tmpSkipBlockComment = do
  ne <- Sc.single "block_comment"
  pure $ TopDiags [ UnhandledNodeDiag "block_comment" (spanNE ne) ]