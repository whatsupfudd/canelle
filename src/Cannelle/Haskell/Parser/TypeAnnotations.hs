module Cannelle.Haskell.Parser.TypeAnnotations where

import Control.Applicative (asum, many, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (void)

import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.TreeSitter.Types (NodeEntry(..))
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Names
import Cannelle.Haskell.Parser.Recovery (spanNE)
import Cannelle.Haskell.Parser.Types


typeSignatureS :: ScannerP TypeAnnotation
typeSignatureS = debugOpt "ts-typeSignature" $ asum [
    debugOpt "ts-forall" $ forallSignatureS
    , debugOpt "ts-context" $ contextSignatureS
    , debugOpt "ts-function" $ functionSignatureS
    , debugOpt "ts-kind" $ kindSignatureS
    , debugOpt "ts-infix" $ infixSignatureS
    , debugOpt "ts-apply" $ applySignatureS
    , debugOpt "ts-prefix" $ prefixTypeSignatureS
    , debugOpt "ts-paren" $ parenSignatureS
    , debugOpt "ts-unit" $ unitSignatureS
    , debugOpt "ts-list" $ listSignatureS
    , debugOpt "ts-tuple" $ tupleSignatureS
    , debugOpt "ts-wildcard" $ wildcardSignatureS
    , debugOpt "ts-literal" $ literalTypeSignatureS
    , debugOpt "ts-name" $ nameSignatureS
    -- , debugOpt "ts-unknown" $ unknownTypeS
  ]


nameSignatureS :: ScannerP TypeAnnotation
nameSignatureS = NameTA <$> debugOpt "ns-nameSignature" identifierS


forallSignatureS :: ScannerP TypeAnnotation
forallSignatureS = do
  S.singleP "forall"
  debugOpt "fa-forall" $ singleAny [ "forall", "∀" ]
  parameters <- debugOpt "fa-variables" $ do
    singlePAny [ "quantified_variables", "quantified_type_variables" ]
    many typeParameterS
  S.single "."
  ForallTA parameters <$> typeSignatureS


contextSignatureS :: ScannerP TypeAnnotation
contextSignatureS = do
  S.singleP "context"
  constraints <- debugOpt "cs-constraints" typeSignatureS
  S.single "=>"
  ContextTA (contextFromAnnotation constraints) <$> typeHeadS
  -- typeSignatureS


typeHeadS :: ScannerP TypeHead
typeHeadS = debugOpt "typeHead-try" $ do
  contextualTypeHeadS <|> directTypeHeadS


contextualTypeHeadS :: ScannerP TypeHead
contextualTypeHeadS = debugOpt "th-ctxtTypeHead" $ do
  annotation <- contextSignatureS
  pure $ case annotation of
    ContextTA context body -> body
      -- TODO: reconciliate with the ContextTA constructor now having a TypeHead.array
      -- fromMaybe emptyTypeHead $ typeHeadFromAnnotation (Just context) body
    _ -> emptyTypeHead


directTypeHeadS :: ScannerP TypeHead
directTypeHeadS = debugOpt "th-directTypeHead" $ do
  typeName <- debugOpt "th-typeName" $ NameIdent <$> S.symbol "name"
  parameters <- fromMaybe [] <$> optional typeParametersS
  pure $ TypeHead typeName parameters Nothing Nothing



functionSignatureS :: ScannerP TypeAnnotation
functionSignatureS = do
  S.singleP "function"
  leftSide <- debugOpt "fs-leftSide" typeSignatureS
  S.single "->"
  FunctionTA leftSide <$> typeSignatureS


kindSignatureS :: ScannerP TypeAnnotation
kindSignatureS = do
  S.singleP "signature"
  typeValue <- debugOpt "ks-typeValue" typeSignatureS
  S.single "::"
  KindTA typeValue <$> typeSignatureS


infixSignatureS :: ScannerP TypeAnnotation
infixSignatureS = do
  S.singleP "infix"
  leftSide <- debugOpt "is-leftSide" typeSignatureS
  operator <- debugOpt "is-operator" typeOperatorS
  InfixTA leftSide operator <$> typeSignatureS


applySignatureS :: ScannerP TypeAnnotation
applySignatureS = do
  S.singleP "apply"
  leftSide <- debugOpt "as-leftSide" typeSignatureS
  ApplyTA leftSide <$> typeSignatureS


prefixTypeSignatureS :: ScannerP TypeAnnotation
prefixTypeSignatureS = do
  singlePAny [ "prefix_id", "prefix_operator" ]
  S.single "("
  operator <- debugOpt "ps-operator" typeOperatorAtomS
  S.single ")"
  pure $ OperatorTA operator


parenSignatureS :: ScannerP TypeAnnotation
parenSignatureS = do
  S.singleP "parens"
  debugOpt "ps-inner" $ S.single "("
  innerSignature <- debugOpt "ps-innerSignature" typeSignatureS
  S.single ")"
  pure $ ParenTA innerSignature


unitSignatureS :: ScannerP TypeAnnotation
unitSignatureS = do
  S.singleP "unit"
  debugOpt "us-unit" $ S.single "("
  S.single ")"
  pure VoidTA


listSignatureS :: ScannerP TypeAnnotation
listSignatureS = do
  S.singleP "list"
  debugOpt "ls-innerList" $ S.single "["
  innerSignature <- debugOpt "ls-innerSignature" typeSignatureS
  S.single "]"
  pure $ ListTA innerSignature


tupleSignatureS :: ScannerP TypeAnnotation
tupleSignatureS = do
  S.singleP "tuple"
  debugOpt "tu-inner" $ S.single "("
  innerSignatures <- debugOpt "tu-innerSignatures" $ typeSignatureS `S.sepBy` S.single ","
  S.single ")"
  pure $ TupleTA innerSignatures


wildcardSignatureS :: ScannerP TypeAnnotation
wildcardSignatureS = WildcardTA <$> S.symbol "wildcard"


literalTypeSignatureS :: ScannerP TypeAnnotation
literalTypeSignatureS = do
  S.singleP "literal"
  LiteralTA <$> asum [
      debugOpt "lt-integer" $ IntegerTL <$> S.symbol "integer"
    , debugOpt "lt-string" $ StringTL <$> S.symbol "string"
    , debugOpt "lt-char" $ CharTL <$> S.symbol "char"
    ]


typeOperatorS :: ScannerP TypeOperator
typeOperatorS = asum [ typeOperatorAtomS, namedTypeOperatorS ]


typeOperatorAtomS :: ScannerP TypeOperator
typeOperatorAtomS = asum [
    OperatorTO <$> S.symbol "operator"
    , ConstructorOperatorTO <$> S.symbol "constructor_operator"
  ]


namedTypeOperatorS :: ScannerP TypeOperator
namedTypeOperatorS = do
  singlePAny [ "infix_id", "infix_type" ]
  _ <- optional $ S.single "`"
  identifier <- identifierS
  _ <- optional $ S.single "`"
  pure $ NamedOperatorTO identifier


typeParametersS :: ScannerP [TypeParameter]
typeParametersS = debugOpt "tp-typeParameters" $ do
  singlePAny [ "type_patterns", "type_parameters", "type_params" ]
  many typeParameterS


typeParameterS :: ScannerP TypeParameter
typeParameterS = debugOpt "tp-typeParameter" $ parameterFromAnnotation <$> typeSignatureS


parameterFromAnnotation :: TypeAnnotation -> TypeParameter
parameterFromAnnotation annotation =
  case stripParens annotation of
    KindTA parameterType parameterKind -> TypeParameter parameterType (Just parameterKind)
    parameterType -> TypeParameter parameterType Nothing


contextFromAnnotation :: TypeAnnotation -> TypeContext
contextFromAnnotation annotation =
  case stripParens annotation of
    TupleTA constraints -> TypeContext constraints
    constraint -> TypeContext [constraint]


typeHeadFromAnnotation :: Maybe TypeContext -> TypeAnnotation -> Maybe TypeHead
typeHeadFromAnnotation mbContext annotation =
  let
    (body, mbKind) = splitKind $ stripParens annotation
    (headType, arguments) = flattenApplication body
  in
  case stripParens headType of
    NameTA identifier -> Just $ TypeHead identifier (map parameterFromAnnotation arguments) mbContext mbKind
    _ -> Nothing

emptyTypeHead :: TypeHead
emptyTypeHead = TypeHead {
  nameTH = NameIdent 1
  , parametersTH = []
  , contextTH = Nothing
  , kindTH = Nothing
}

flattenApplication :: TypeAnnotation -> (TypeAnnotation, [TypeAnnotation])
flattenApplication annotation =
  case stripParens annotation of
    ApplyTA leftSide rightSide ->
      let (headType, arguments) = flattenApplication leftSide
      in (headType, arguments <> [rightSide])
    other -> (other, [])


splitKind :: TypeAnnotation -> (TypeAnnotation, Maybe TypeAnnotation)
splitKind annotation =
  case annotation of
    KindTA typeValue typeKind -> (typeValue, Just typeKind)
    other -> (other, Nothing)


stripParens :: TypeAnnotation -> TypeAnnotation
stripParens annotation =
  case annotation of
    ParenTA inner -> stripParens inner
    other -> other


unknownTypeS :: ScannerP TypeAnnotation
unknownTypeS = do
  ne <- S.anyNode
  pure $ UnknownTA ne.name (spanNE ne)


singleAny :: [String] -> ScannerP ()
singleAny nodeNames = void $ asum (map S.single nodeNames)


singlePAny :: [String] -> ScannerP ()
singlePAny nodeNames = void $ asum (map S.singleP nodeNames)
