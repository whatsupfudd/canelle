module Cannelle.Elm.Parser.TypeExpr
  ( scanTE
  , scanAtomTE
  , scanVarTE
  , scanRefTE
  , scanRecordTE
  , scanFieldTE
  , scanTupleTE
  , scanParenTE
  , scanUnitTE
  , mkFunTE
  ) where

import Control.Applicative (many, optional, (<|>))

import Data.Foldable (asum)
import qualified Data.Set as E
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types (NodeEntry (..), SegmentPos (..))

import Cannelle.Elm.AST
import qualified Cannelle.Elm.Parser.Module as ModP
import Cannelle.Elm.Parser.Types (ParserP)
import Cannelle.Elm.Parser.Utils (syntheticSpan, spanNE)


scanTE :: ParserP TypeExpr
scanTE = do
  _ <- Sc.singleP "type_expression"
  parts <- scanAtomTE `Sc.sepBy1` (Sc.single "arrow")
  case parts of
    [] -> Sc.failure "@[elm/type-expr] empty type_expression" Nothing E.empty
    [one] -> pure one
    manyParts -> pure $ mkFunTE manyParts

mkFunTE :: [TypeExpr] -> TypeExpr
mkFunTE parts =
  case parts of
    [] -> error "@[elm/type-expr] mkFunTE: empty list"
    [one] -> one
    first : rest -> FunTE first (mkFunTE rest)

scanAtomTE :: ParserP TypeExpr
scanAtomTE =
  asum
    [ scanRefTE
    , scanRecordTE
    , scanParenOrUnitTE
    , scanTupleTE
    , scanVarTE
    -- , scanUnhandledTE
    ]

scanVarTE :: ParserP TypeExpr
scanVarTE = do
  _ <- Sc.singleP "type_variable"
  name <- scanTypeVarName
  pure $ VarTE name

scanTypeVarName :: ParserP Name
scanTypeVarName =
  asum
    [ scanLowerTypeName
    , ModP.scanLowerName
    ]

scanLowerTypeName :: ParserP Name
scanLowerTypeName =
  scanSymbolAsName "lower_type_name"

scanRefTE :: ParserP TypeExpr
scanRefTE = do
  _ <- Sc.singleP "type_ref"
  refName <- scanTypeRefName
  args <- many scanAtomTE
  pure $ RefTE refName (V.fromList args)

scanTypeRefName :: ParserP QName
scanTypeRefName = asum [
    ModP.scanQName
  , QName . V.singleton <$> ModP.scanUpperName
  ]

scanRecordTE :: ParserP TypeExpr
scanRecordTE = do
  _ <- Sc.singleP "record_type"
  _ <- Sc.single "{"
  ext <- optional scanRecordExt
  fields <- scanFieldTE `Sc.sepBy` Sc.single ","
  _ <- Sc.single "}"
  pure $ RecordTE ext (V.fromList fields)

scanRecordExt :: ParserP Name
scanRecordExt = do
  base <-
    asum
      [ scanRecordBaseName
      , ModP.scanLowerName
      ]
  _ <- Sc.single "|"
  pure base

scanRecordBaseName :: ParserP Name
scanRecordBaseName = do
  _ <- Sc.singleP "record_base_identifier"
  ModP.scanLowerName

scanFieldTE :: ParserP TypeField
scanFieldTE = do
  ne <- Sc.singleP "field_type"
  name <- ModP.scanLowerName
  _ <- Sc.single "colon"
  expr <- scanTE
  _ <- optional $ Sc.single "line_comment"
  pure $
    TypeField
      { nameTF = name
      , exprTF = expr
      , spanTF = spanNE ne
      }

scanTupleTE :: ParserP TypeExpr
scanTupleTE = do
  _ <- Sc.singleP "tuple_type"
  _ <- Sc.single "("
  items <- scanTE `Sc.sepBy1` Sc.single ","
  _ <- Sc.single ")"
  pure $ TupleTE (V.fromList items)

scanParenOrUnitTE :: ParserP TypeExpr
scanParenOrUnitTE =
  scanUnitTE <|> scanParenTE

scanUnitTE :: ParserP TypeExpr
scanUnitTE = do
  open <- Sc.single "("
  close <- Sc.single ")"
  pure $ UnitTE (mergeSpans (spanNE open) (spanNE close))

scanParenTE :: ParserP TypeExpr
scanParenTE = do
  _ <- Sc.single "("
  inner <- scanTE
  _ <- Sc.single ")"
  pure $ ParenTE inner

scanUnhandledTE :: ParserP TypeExpr
scanUnhandledTE = do
  ne <- anyNode
  pure $ UnhandledTE (T.pack ne.name) (spanNE ne)

anyNode :: ParserP NodeEntry
anyNode =
  Sc.pToken Just mempty

-- TODO(elm-name-text):
-- Replace placeholder textName values with real source-sliced identifier text
-- once the parser state or a later resolver step can recover the source text
-- from demand spans.
scanSymbolAsName :: String -> ParserP Name
scanSymbolAsName nodeName = do
  demand <- Sc.symbol nodeName
  pure $
    Name
      { textName = placeholderText demand
      , spanName = syntheticSpan
      , demandName = Just demand
      }

placeholderText :: Int -> T.Text
placeholderText demand =
  "#" <> T.pack (show demand)

mergeSpans :: SegmentPos -> SegmentPos -> SegmentPos
mergeSpans (startSp, _) (_, endSp) =
  (startSp, endSp)
