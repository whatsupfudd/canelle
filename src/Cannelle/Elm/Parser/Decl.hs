module Cannelle.Elm.Parser.Decl
  ( scanDecl
  , scanAlias
  , scanUnion
  , scanCtor
  , scanTypeParams
  , scanTypeParam
  , scanCtorArg
  , scanAnn
  , scanVal
  , scanValLeft
  , scanPattern
  , scanNamePattern
  , scanIgnoredPattern
  , scanTuplePattern
  , scanRecordPattern
  , scanUnhandledPattern
  , scanUnhandledDecl
  ) where

import Control.Applicative ((<|>), many)
import Data.Foldable (asum)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types (SegmentPos, NodeEntry(..))
import TreeSitter.Node (TSPoint(..))

import Cannelle.Elm.AST
import qualified Cannelle.Elm.Parser.Module as ModP
import qualified Cannelle.Elm.Parser.TypeExpr as Tep
import Cannelle.Elm.Parser.Types (ParserP)
import Cannelle.Elm.Parser.Utils (spanNE)

scanDecl :: ParserP Decl
scanDecl =
  asum
    [ AliasD <$> scanAlias
    , UnionD <$> scanUnion
    , AnnotationD <$> scanAnn
    , ValueD <$> scanVal
    , PortD <$> scanPort
    , scanUnhandledDecl
    ]

scanAlias :: ParserP AliasDecl
scanAlias = do
  ne <- Sc.singleP "type_alias_declaration"
  _ <- Sc.single "type"
  _ <- Sc.single "alias"
  name <- ModP.scanUpperName
  params <- scanTypeParams
  _ <- Sc.single "eq" <|> Sc.single "="
  expr <- Tep.scanTE
  pure AliasDecl { nameAlias = name, paramsAlias = params, exprAlias = expr, spanAlias = (ne.start, ne.end) }

scanUnion :: ParserP UnionDecl
scanUnion = do
  ne <- Sc.singleP "type_declaration"
  _ <- Sc.single "type"
  name <- ModP.scanUpperName
  params <- scanTypeParams
  _ <- Sc.single "eq" <|> Sc.single "="
  ctors <- scanUnionCtors
  pure UnionDecl { nameUnion = name, paramsUnion = params, ctorsUnion = V.fromList ctors
        , spanUnion = (ne.start, ne.end) }


scanAnn :: ParserP AnnotationDecl
scanAnn = do
  ne <- Sc.singleP "type_annotation"
  name <- ModP.scanLowerName
  _ <- Sc.single "colon"
  expr <- Tep.scanTE
  pure AnnotationDecl { nameAnn = name, exprAnn = expr, spanAnn = (ne.start, ne.end) }


scanVal :: ParserP ValueDecl
scanVal = do
  ne <- Sc.singleP "value_declaration"
  (name, args) <- scanValLeft
  _ <- Sc.single "eq" <|> Sc.single "="
  bodySp <- scanValBody
  pure $
    ValueDecl
      { nameVal = name
      , argsVal = args
      , spanBodyVal = bodySp
      , spanVal = (ne.start, ne.end)
      }

scanValLeft :: ParserP (Name, V.Vector PatternSummary)
scanValLeft =
  scanValLeftNode <|> scanValLeftFlat

scanValLeftNode :: ParserP (Name, V.Vector PatternSummary)
scanValLeftNode = do
  Sc.singleP "function_declaration_left"
  name <- ModP.scanLowerName
  args <- many scanPattern
  pure (name, V.fromList args)

scanValLeftFlat :: ParserP (Name, V.Vector PatternSummary)
scanValLeftFlat = do
  name <- ModP.scanLowerName
  args <- many scanPattern
  pure (name, V.fromList args)

scanValBody :: ParserP SegmentPos
scanValBody = do
  _ <- many (Sc.single "line_comment" <|> Sc.single "block_comment")
  asum $ fmap scanBodyAs bodyNodeNames

scanBodyAs :: String -> ParserP SegmentPos
scanBodyAs nodeName = do
  ne <- Sc.single nodeName
  pure $ spanNE ne


bodyNodeNames :: [String]
bodyNodeNames =
  [ "if_else_expr"
  , "case_of_expr"
  , "let_in_expr"
  , "lambda_expr"
  , "anonymous_function_expr"
  , "function_call_expr"
  , "operator_application"
  , "bin_op_expr"
  , "negate_expr"
  , "field_access_expr"
  , "record_access_expr"
  , "record_expr"
  , "record_update_expr"
  , "list_expr"
  , "tuple_expr"
  , "parenthesized_expr"
  , "value_expr"
  , "unit_expr"
  , "number_constant_expr"
  , "char_constant_expr"
  , "string_constant_expr"
  , "glsl_expression"
  , "lower_case_identifier"
  , "upper_case_identifier"
  , "upper_case_qid"
  ]

scanPattern :: ParserP PatternSummary
scanPattern =
  asum
    [ scanNamePattern
    , scanIgnoredPattern
    , scanTuplePattern
    , scanRecordPattern
    , scanUnhandledPattern
    ]

scanNamePattern :: ParserP PatternSummary
scanNamePattern =
  asum
    [ do
        Sc.singleP "lower_pattern"
        NamePS <$> ModP.scanLowerName
    , NamePS <$> ModP.scanLowerName
    ]

scanIgnoredPattern :: ParserP PatternSummary
scanIgnoredPattern =
  asum
    [ ignoredAs "anything_pattern"
    , ignoredAs "underscore_pattern"
    , ignoredAs "_"
    ]

ignoredAs :: String -> ParserP PatternSummary
ignoredAs nodeName = do
  ne <- Sc.single nodeName
  pure $ IgnoredPS (spanNE ne)

scanTuplePattern :: ParserP PatternSummary
scanTuplePattern = do
  Sc.singleP "tuple_pattern"
  _ <- Sc.single "("
  items <- scanPattern `Sc.sepBy1` Sc.single ","
  _ <- Sc.single ")"
  pure $
    TuplePS
      (V.fromList items)
      (spanPatterns items)

scanRecordPattern :: ParserP PatternSummary
scanRecordPattern = do
  Sc.singleP "record_pattern"
  _ <- Sc.single "{"
  names <- ModP.scanLowerName `Sc.sepBy` Sc.single ","
  _ <- Sc.single "}"
  pure $
    RecordPS
      (V.fromList names)
      (spanNames names)

scanUnhandledPattern :: ParserP PatternSummary
scanUnhandledPattern =
  asum
    [ unhandledPatternAs "constructor_pattern"
    , unhandledPatternAs "upper_pattern"
    , unhandledPatternAs "list_pattern"
    , unhandledPatternAs "cons_pattern"
    , unhandledPatternAs "alias_pattern"
    , unhandledPatternAs "as_pattern"
    , unhandledPatternAs "parenthesized_pattern"
    , unhandledPatternAs "unit_pattern"
    , unhandledPatternAs "string_constant_pattern"
    , unhandledPatternAs "number_constant_pattern"
    , unhandledPatternAs "char_constant_pattern"
    ]

unhandledPatternAs :: String -> ParserP PatternSummary
unhandledPatternAs nodeName = do
  ne <- Sc.single nodeName
  pure $
    UnhandledPS
      (T.pack nodeName)
      (spanNE ne)

scanUnionCtors :: ParserP [Ctor]
scanUnionCtors =
  asum
    [ scanCtorList "union_variant_list"
    , scanCtorList "union_variants"
    , scanCtorList "union_constructors"
    , scanCtor `Sc.sepBy1` scanCtorSep
    ]

scanCtorList :: String -> ParserP [Ctor]
scanCtorList nodeName = do
  Sc.singleP nodeName
  scanCtor `Sc.sepBy1` scanCtorSep

scanCtorSep :: ParserP ()
scanCtorSep = do
  _ <- Sc.single "pipe" <|> Sc.single "|"
  pure ()

scanTypeParams :: ParserP (V.Vector Name)
scanTypeParams =
  V.fromList <$> many scanTypeParam

scanTypeParam :: ParserP Name
scanTypeParam =
  scanLowerTypeName <|> ModP.scanLowerName

scanLowerTypeName :: ParserP Name
scanLowerTypeName =
  scanSymbolAsName "lower_type_name"
    <|> do
      Sc.singleP "lower_type_name"
      ModP.scanLowerName

scanCtor :: ParserP Ctor
scanCtor =
  scanCtorNode <|> scanCtorFlat

scanCtorNode :: ParserP Ctor
scanCtorNode =
  asum
    [ scanCtorNodeAs "union_variant"
    , scanCtorNodeAs "union_constructor"
    , scanCtorNodeAs "constructor"
    , scanCtorNodeAs "type_constructor"
    ]

scanCtorNodeAs :: String -> ParserP Ctor
scanCtorNodeAs nodeName = do
  Sc.singleP nodeName
  name <- ModP.scanUpperName
  args <- many scanCtorArg
  pure $
    Ctor
      { nameCtor = name
      , argsCtor = V.fromList args
      , spanCtor = spanCtorFrom name args
      }

scanCtorFlat :: ParserP Ctor
scanCtorFlat = do
  name <- ModP.scanUpperName
  args <- many scanCtorArg
  pure $
    Ctor
      { nameCtor = name
      , argsCtor = V.fromList args
      , spanCtor = spanCtorFrom name args
      }

scanCtorArg :: ParserP TypeExpr
scanCtorArg =
  Tep.scanTE <|> Tep.scanAtomTE


scanPort :: ParserP PortDecl
scanPort = do
  ne <- Sc.singleP "port_annotation"
  Sc.single "port"
  name <- ModP.scanLowerName
  Sc.single "colon"
  expr <- Tep.scanTE

  pure $ PortDecl { namePort = name, exprPort = expr, spanPort = spanNE ne }


scanUnhandledDecl :: ParserP Decl
scanUnhandledDecl =
  asum
    [ unhandledAs "type_annotation"
    , unhandledAs "value_declaration"
    , unhandledAs "port_annotation"
    , unhandledAs "infix_declaration"
    ]

unhandledAs :: String -> ParserP Decl
unhandledAs nodeName = do
  ne <- Sc.single nodeName
  pure $ UnhandledD (T.pack nodeName) (spanNE ne)

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

spanAliasFrom :: Name -> TypeExpr -> SegmentPos
spanAliasFrom name expr =
  mergeSpans name.spanName (spanTE expr)

spanAnnFrom :: Name -> TypeExpr -> SegmentPos
spanAnnFrom name expr =
  mergeSpans name.spanName (spanTE expr)

spanValFrom :: Name -> SegmentPos -> SegmentPos
spanValFrom name bodySp =
  mergeSpans name.spanName bodySp

spanUnionFrom :: Name -> [Ctor] -> SegmentPos
spanUnionFrom name ctors =
  case reverse ctors of
    [] -> name.spanName
    lastCtor : _ -> mergeSpans name.spanName lastCtor.spanCtor

spanCtorFrom :: Name -> [TypeExpr] -> SegmentPos
spanCtorFrom name args =
  case reverse args of
    [] -> name.spanName
    lastArg : _ -> mergeSpans name.spanName (spanTE lastArg)

spanPattern :: PatternSummary -> SegmentPos
spanPattern pattern =
  case pattern of
    NamePS name -> name.spanName
    IgnoredPS sp -> sp
    TuplePS _ sp -> sp
    RecordPS _ sp -> sp
    UnhandledPS _ sp -> sp

spanPatterns :: [PatternSummary] -> SegmentPos
spanPatterns patterns =
  case patterns of
    [] -> syntheticSpan
    firstPat : rest ->
      let lastPat = foldl (\_ cur -> cur) firstPat rest
      in mergeSpans (spanPattern firstPat) (spanPattern lastPat)

spanNames :: [Name] -> SegmentPos
spanNames names =
  case names of
    [] -> syntheticSpan
    firstNm : rest ->
      let lastNm = foldl (\_ cur -> cur) firstNm rest
      in mergeSpans firstNm.spanName lastNm.spanName

spanTE :: TypeExpr -> SegmentPos
spanTE expr =
  case expr of
    VarTE name ->
      name.spanName

    RefTE qn args ->
      if V.null args
        then spanQName qn
        else
          let lastArg = V.last args
          in mergeSpans (spanQName qn) (spanTE lastArg)

    RecordTE mbBase fields ->
      case (mbBase, V.null fields) of
        (Just base, True) ->
          base.spanName

        (Just base, False) ->
          let lastField = V.last fields
          in mergeSpans base.spanName lastField.spanTF

        (Nothing, False) ->
          let firstField = V.head fields
              lastField = V.last fields
          in mergeSpans firstField.spanTF lastField.spanTF

        (Nothing, True) ->
          syntheticSpan

    TupleTE items ->
      if V.null items
        then syntheticSpan
        else
          let firstItem = V.head items
              lastItem = V.last items
          in mergeSpans (spanTE firstItem) (spanTE lastItem)

    FunTE left right ->
      mergeSpans (spanTE left) (spanTE right)

    ParenTE inner ->
      spanTE inner

    UnitTE sp ->
      sp

    UnhandledTE _ sp ->
      sp

spanQName :: QName -> SegmentPos
spanQName qn =
  if V.null qn.partsQName
    then syntheticSpan
    else
      let firstNm = V.head qn.partsQName
          lastNm = V.last qn.partsQName
      in mergeSpans firstNm.spanName lastNm.spanName

mergeSpans :: SegmentPos -> SegmentPos -> SegmentPos
mergeSpans (startSp, _) (_, endSp) =
  (startSp, endSp)

syntheticSpan :: SegmentPos
syntheticSpan =
  (syntheticPoint, syntheticPoint)

syntheticPoint :: TSPoint
syntheticPoint =
  TSPoint 0 0