-- src/Cannelle/Haskell/Parser/Recovery.hs

module Cannelle.Haskell.Parser.Recovery where

import Control.Applicative (asum)
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.TreeSitter.Types (SegmentPos (..), NodeEntry(..))

import Cannelle.Parser.Debug (debugOpt)
import Cannelle.TreeSitter.Types ( NodeEntry, SegmentPos )

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Types


-- | Parse one node that the Haskell scanner does not currently understand.
--
-- The specifically named alternatives are retained because they produce
-- clearer debug traces for common recovery cases. The final alternative makes
-- the parser total for the current scanner level.
unknownDeclS :: ScannerP Declaration
unknownDeclS = asum $ map unhandledAs [
    "ERROR", "haddock", "pragma"
    , "instance", "default", "foreign_import", "foreign_export"
    , "fixity", "role_annotation", "standalone_deriving"
    , "pattern_synonym", "splice", "cpp", "preprocessor"
    , "function", "bind", "top_splice", "signature"
    , "data_type", "newtype", "type_synomym", "class"
    , "type_family", "data_family", "import"
  ]


-- | Consume a known but currently unsupported node without entering it.
unhandledAs :: String -> ScannerP Declaration
unhandledAs nodeName = debugOpt ("ud-" <> nodeName) $ do
  ne <- S.single nodeName
  pure $ unknownFromNode ne


-- | Consume any remaining node without entering it.
--
-- This alternative must always be placed last in a dispatcher.
unhandledNodeS :: ScannerP Declaration
unhandledNodeS = debugOpt "ud-unknownDecl" $ do
  ne <- S.anyNode
  pure $ unknownFromNode ne


unknownFromNode :: NodeEntry -> Declaration
unknownFromNode ne =
  UnknownDC ne.name (spanNE ne)


spanNE :: NodeEntry -> SegmentPos
spanNE ne = (ne.start, ne.end)


-- | Collect every explicit Tree-Sitter ERROR node, including ERROR nodes
-- nested inside a declaration that was recovered as one opaque parent node.
collectSyntaxErrorDecls :: [NodeEntry] -> V.Vector Declaration
collectSyntaxErrorDecls = V.fromList . concatMap syntaxErrorsIn


syntaxErrorsIn :: NodeEntry -> [Declaration]
syntaxErrorsIn ne
  | ne.name == "ERROR" = [UnknownDC "ERROR" (spanNE ne)]
  | otherwise = concatMap syntaxErrorsIn ne.children


-- | Add ERROR markers that were not already emitted directly by the scanner.
--
-- Directly encountered ERROR nodes become UnknownDC values through
-- unknownDeclS. Nested ERROR nodes may instead be hidden inside an opaque
-- recovered parent, so the original NodeEntry graph is also inspected.
appendSyntaxErrors :: V.Vector Declaration -> V.Vector Declaration -> V.Vector Declaration
appendSyntaxErrors = V.foldl' appendSyntaxError


appendSyntaxError :: V.Vector Declaration -> Declaration -> V.Vector Declaration
appendSyntaxError declarations declaration =
  case declaration of
    UnknownDC "ERROR" segment
      | containsSyntaxError segment declarations -> declarations
      | otherwise -> V.snoc declarations declaration
    _ -> declarations


containsSyntaxError :: SegmentPos -> V.Vector Declaration -> Bool
containsSyntaxError segment =
  V.any $ \declaration ->
    case declaration of
      UnknownDC "ERROR" currentSegment -> currentSegment == segment
      _ -> False