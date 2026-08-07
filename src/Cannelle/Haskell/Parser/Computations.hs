module Cannelle.Haskell.Parser.Computations where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (void, when)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Names (identifierS, qualifiedModuleNameS)
import Cannelle.Haskell.Parser.Recovery (spanNE)
import Cannelle.Haskell.Parser.TypeAnnotations (typeSignatureS)
import Cannelle.Haskell.Parser.Types


-- ************* FUNCTIONS AND BINDINGS *************

functionDeclS :: ScannerP FunctionContent
functionDeclS = debugOpt "fn-function" $ do
  S.singleP "function"
  functionName <- functionNameS
  patterns <- fromMaybe V.empty <$> optional patternsS
  matches <- some matchS
  localBindings <- fromMaybe [] <$> optional whereBindingsS
  pure $ FunctionContent functionName patterns matches localBindings


functionNameS :: ScannerP FunctionName
functionNameS = asum [
    VariableFN <$> S.symbol "variable"
    , OperatorFN <$> prefixOperatorS
  ]


patternBindingS :: ScannerP PatternBinding
patternBindingS = debugOpt "bd-patternBinding" $ do
  S.singleP "bind"
  bindingPattern <- patternS
  matches <- some matchS
  localBindings <- fromMaybe [] <$> optional whereBindingsS
  pure $ PatternBinding bindingPattern matches localBindings


topSpliceS :: ScannerP Expression
topSpliceS = do
  S.singleP "top_splice"
  expressionS


patternsS :: ScannerP (V.Vector Pattern)
patternsS = do
  S.singleP "patterns"
  V.fromList <$> many patternS


matchS :: ScannerP MatchContent
matchS = do
  (guards, value) <- matchValueS "="
  pure $ MatchContent guards value


matchValueS :: String -> ScannerP ([GuardContent], Expression)
matchValueS separator = do
  S.singleP "match"
  mbGuard <- optional $ S.single "|"
  guards <- if isJust mbGuard then guardsS else pure []
  S.single separator
  value <- expressionS
  pure (guards, value)


guardsS :: ScannerP [GuardContent]
guardsS = do
  S.singleP "guards"
  guardS `S.sepBy` S.single ","


guardS :: ScannerP GuardContent
guardS = asum [
    patternGuardS, letGuardS, booleanGuardS, directBooleanGuardS
    , unknownGuardAs "ERROR", unknownGuardAs "pragma"
  ]


patternGuardS :: ScannerP GuardContent
patternGuardS = do
  S.singleP "pattern_guard"
  bindingPattern <- patternS
  S.single "<-"
  PatternGuardGC bindingPattern <$> expressionS


letGuardS :: ScannerP GuardContent
letGuardS = do
  S.singleP "let"
  _ <- optional $ S.single "let"
  LetGuardGC <$> localBindingsS


booleanGuardS :: ScannerP GuardContent
booleanGuardS = do
  S.singleP "boolean"
  BooleanGuardGC <$> expressionS


directBooleanGuardS :: ScannerP GuardContent
directBooleanGuardS = BooleanGuardGC <$> expressionS


unknownGuardAs :: String -> ScannerP GuardContent
unknownGuardAs nodeName = do
  ne <- S.single nodeName
  pure $ UnknownGuardGC nodeName (spanNE ne)


whereBindingsS :: ScannerP [LocalBinding]
whereBindingsS = S.single "where" *> localBindingsS


localBindingsS :: ScannerP [LocalBinding]
localBindingsS = do
  prefixComment <- many $ S.symbol "comment"
  S.singleP "local_binds"
  many localBindingS


localBindingS :: ScannerP LocalBinding
localBindingS = asum [
    LocalFunctionLB <$> functionDeclS
    , LocalPatternLB <$> patternBindingS
    , localSignatureS
    , LocalCommentLB <$> S.symbol "comment"
    , unknownLocalBindingAs "haddock"
    , unknownLocalBindingAs "pragma"
    , unknownLocalBindingAs "ERROR"
    , unknownLocalBindingAs "fixity"
  ]


localSignatureS :: ScannerP LocalBinding
localSignatureS = do
  S.singleP "signature"
  firstName <- signatureNameS
  additionalNames <- many $ S.single "," *> signatureNameS
  S.single "::"
  LocalSignatureLB (V.fromList $ firstName : additionalNames) <$> typeSignatureS


signatureNameS :: ScannerP SignatureName
signatureNameS = asum [
    prefixSignatureNameS
    , VariableSN <$> S.symbol "variable"
    , TypeSN <$> S.symbol "name"
    , ConstructorSN <$> S.symbol "constructor"
    , OperatorSN <$> S.symbol "operator"
    , ConstructorOperatorSN <$> S.symbol "constructor_operator"
  ]


prefixSignatureNameS :: ScannerP SignatureName
prefixSignatureNameS = do
  operator <- prefixOperatorS
  pure $ case operator of
    VariableOP anInt -> OperatorSN anInt
    ConstructorOP anInt -> ConstructorOperatorSN anInt
    QualifiedOP _ -> OperatorSN 0
    BackquotedOP _ -> OperatorSN 0


unknownLocalBindingAs :: String -> ScannerP LocalBinding
unknownLocalBindingAs nodeName = do
  ne <- S.single nodeName
  pure $ LocalUnknownLB nodeName (spanNE ne)


-- ************* PATTERNS *************

patternS :: ScannerP Pattern
patternS = debugOpt "pt-pattern" $ do
  openingComments <- many $ S.symbol "comment"
  pattern <- asum [
    debugOpt "pt-apply" applyPatternS
    , debugOpt "pt-infix" infixPatternS
    , debugOpt "pt-record" recordPatternS
    , debugOpt "pt-view" viewPatternS
    , debugOpt "pt-as" asPatternS
    , debugOpt "pt-irrefutable" irrefutablePatternS
    , debugOpt "pt-strict" strictPatternS
    , debugOpt "pt-signature" patternSignatureS
    , debugOpt "pt-paren" parenPatternS
    , debugOpt "pt-tuple" tuplePatternS
    , debugOpt "pt-list" listPatternS
    , debugOpt "pt-unit" unitPatternS
    , debugOpt "pt-negative" negativePatternS
    , debugOpt "pt-literal" literalPatternS
    , debugOpt "pt-wildcard" wildcardPatternS
    , debugOpt "pt-qualified" qualifiedPatternS
    , debugOpt "pt-constructor" constructorPatternS
    , debugOpt "pt-variable" variablePatternS
    , debugOpt "pt-recover" recoverPatternS
    ]
  trailingComments <- many $ S.symbol "comment"
  pure $ PatternWithComments pattern openingComments trailingComments


variablePatternS :: ScannerP Pattern
variablePatternS = VariablePT <$> S.symbol "variable"


constructorPatternS :: ScannerP Pattern
constructorPatternS = ConstructorPT . NameIdent <$> S.symbol "constructor"


qualifiedPatternS :: ScannerP Pattern
qualifiedPatternS = ConstructorPT <$> qualifiedTermS


applyPatternS :: ScannerP Pattern
applyPatternS = do
  S.singleP "apply"
  leftSide <- patternS
  ApplyPT leftSide <$> patternS


infixPatternS :: ScannerP Pattern
infixPatternS = do
  S.singleP "infix"
  leftSide <- patternS
  operator <- operatorS
  InfixPT leftSide operator <$> patternS


literalPatternS :: ScannerP Pattern
literalPatternS = do
  S.singleP "literal"
  LiteralPT <$> literalValueS


negativePatternS :: ScannerP Pattern
negativePatternS = do
  singlePAny [ "negative", "negation" ]
  _ <- optional $ S.single "-"
  value <- (S.singleP "literal" *> literalValueS) <|> literalValueS
  pure $ NegativePT value


wildcardPatternS :: ScannerP Pattern
wildcardPatternS = WildcardPT <$> S.symbol "wildcard"


parenPatternS :: ScannerP Pattern
parenPatternS = do
  S.singleP "parens"
  S.single "("
  inner <- patternS
  S.single ")"
  pure $ ParenPT inner


tuplePatternS :: ScannerP Pattern
tuplePatternS = do
  S.singleP "tuple"
  S.single "("
  patterns <- patternS `S.sepBy` S.single ","
  S.single ")"
  pure $ TuplePT patterns


listPatternS :: ScannerP Pattern
listPatternS = do
  S.singleP "list"
  S.single "["
  patterns <- patternS `S.sepBy` S.single ","
  S.single "]"
  pure $ ListPT patterns


unitPatternS :: ScannerP Pattern
unitPatternS = do
  S.singleP "unit"
  S.single "("
  S.single ")"
  pure UnitPT


asPatternS :: ScannerP Pattern
asPatternS = do
  singlePAny [ "as", "as_pattern" ]
  variable <- S.symbol "variable"
  S.single "@"
  AsPT variable <$> patternS


irrefutablePatternS :: ScannerP Pattern
irrefutablePatternS = do
  singlePAny [ "irrefutable", "lazy_pattern" ]
  S.single "~"
  IrrefutablePT <$> patternS


strictPatternS :: ScannerP Pattern
strictPatternS = do
  singlePAny [ "strict", "bang_pattern" ]
  S.single "!"
  StrictPT <$> patternS


viewPatternS :: ScannerP Pattern
viewPatternS = do
  singlePAny [ "view_pattern", "view" ]
  view <- expressionS
  S.single "->"
  ViewPT view <$> patternS


patternSignatureS :: ScannerP Pattern
patternSignatureS = do
  S.singleP "signature"
  inner <- patternS
  S.single "::"
  PatternSignaturePT inner <$> typeSignatureS


recordPatternS :: ScannerP Pattern
recordPatternS = do
  S.singleP "record"
  constructor <- patternS
  RecordPT constructor <$> recordItemsS recordPatternFieldS


recordPatternFieldS :: ScannerP RecordPatternField
recordPatternFieldS =
  recordWildcardPatternS <|> do
    singlePAny [ "field_pattern", "field" ]
    fieldName <- fieldNameS
    value <- optional $ S.single "=" *> patternS
    pure $ FieldPatternRPF fieldName value


recordWildcardPatternS :: ScannerP RecordPatternField
recordWildcardPatternS = asum [
    S.single "all_fields" $> RecordWildcardRPF
    , S.single ".." $> RecordWildcardRPF
  ]


recoverPatternS :: ScannerP Pattern
recoverPatternS = asum $ map unknownPatternAs [
    "apply", "infix", "record", "view_pattern", "view", "as", "as_pattern"
    , "irrefutable", "lazy_pattern", "strict", "bang_pattern", "signature"
    , "parens", "tuple", "list", "unit", "negative", "negation", "literal"
    , "qualified", "constructor", "variable", "wildcard", "splice", "ERROR"
  ]


unknownPatternAs :: String -> ScannerP Pattern
unknownPatternAs nodeName = do
  ne <- S.single nodeName
  pure $ UnknownPT nodeName (spanNE ne)


-- ************* EXPRESSIONS *************

expressionS :: ScannerP Expression
expressionS = debugOpt "ex-expression" $ do
  openingComments <- many $ S.symbol "comment"
  expr <- asum [
    debugOpt "ex-infix" infixExpressionS
    , debugOpt "ex-apply" applyExpressionS
    , debugOpt "ex-projection" projectionExpressionS
    , debugOpt "ex-record" recordExpressionS
    , debugOpt "ex-lambda" lambdaExpressionS
    , debugOpt "ex-case" caseExpressionS
    , debugOpt "ex-ifThenElse" ifThenElseS
    , debugOpt "ex-do" doExpressionS
    , debugOpt "ex-letIn" letInExpressionS
    , debugOpt "ex-leftSection" leftSectionS
    , debugOpt "ex-rightSection" rightSectionS
    , debugOpt "ex-signature" signatureExpressionS
    , debugOpt "ex-negative" negativeExpressionS
    , debugOpt "ex-quasiquote" quasiquoteExpressionS
    , debugOpt "ex-prefixOperator" prefixOperatorExpressionS
    , debugOpt "ex-paren" parenExpressionS
    , debugOpt "ex-list" listExpressionS
    , debugOpt "ex-tuple" tupleExpressionS
    , debugOpt "ex-unit" unitExpressionS
    , debugOpt "ex-literal" literalExpressionS
    , debugOpt "ex-qualified" qualifiedExpressionS
    , debugOpt "ex-constructor" constructorExpressionS
    , debugOpt "ex-variable" variableExpressionS
    , debugOpt "ex-recover" recoverExpressionS
    ]
  trailingComments <- many $ S.symbol "comment"
  pure $ ExprWithComments expr openingComments trailingComments


variableExpressionS :: ScannerP Expression
variableExpressionS = VariableEX <$> S.symbol "variable"


constructorExpressionS :: ScannerP Expression
constructorExpressionS = ConstructorEX <$> S.symbol "constructor"


qualifiedExpressionS :: ScannerP Expression
qualifiedExpressionS = QualifiedEX <$> qualifiedTermS


applyExpressionS :: ScannerP Expression
applyExpressionS = do
  S.singleP "apply"
  leftSide <- expressionS
  ApplyEX leftSide <$> expressionS


infixExpressionS :: ScannerP Expression
infixExpressionS = do
  S.singleP "infix"
  leftSide <- expressionS
  operator <- operatorS
  InfixEX leftSide operator <$> expressionS


projectionExpressionS :: ScannerP Expression
projectionExpressionS = do
  S.singleP "projection"
  prefix <- expressionS
  S.single "."
  ProjectionEX prefix <$> fieldNameS


recordExpressionS :: ScannerP Expression
recordExpressionS = do
  S.singleP "record"
  base <- expressionS
  RecordEX base <$> recordItemsS recordFieldS


recordFieldS :: ScannerP RecordField
recordFieldS = do
  singlePAny [ "field_update", "field" ]
  fieldName <- fieldNameS
  value <- optional $ S.single "=" *> expressionS
  pure $ RecordField fieldName value


lambdaExpressionS :: ScannerP Expression
lambdaExpressionS = do
  S.singleP "lambda"
  singleAny [ "\\", "λ" ]
  patterns <- patternsS <|> (V.fromList <$> some patternS)
  S.single "->"
  LambdaEX patterns <$> expressionS


caseExpressionS :: ScannerP Expression
caseExpressionS = do
  S.singleP "case"
  debugOpt "ex-kw:case" $ S.single "case"
  value <- expressionS
  debugOpt "ex-kw:case" $ S.single "of"
  S.singleP "alternatives"
  CaseEX value <$> many (alternativeS <|> commentAlternativeS)


alternativeS :: ScannerP AlternativeCmt
alternativeS = do
  S.singleP "alternative"
  alternativePattern <- patternS
  debugOpt ("alt-pat: " <> show alternativePattern) $ pure ()
  guardedValues <- some $ matchValueS "->"
  localBindings <- fromMaybe [] <$> optional whereBindingsS
  pure . RealAlternative $ Alternative alternativePattern guardedValues localBindings


commentAlternativeS :: ScannerP AlternativeCmt
commentAlternativeS = do
  CommentAlternative <$> S.symbol "comment"

ifThenElseS :: ScannerP Expression
ifThenElseS = do
  S.singleP "conditional"
  S.single "if"
  condition <- expressionS
  S.single "then"
  thenValue <- expressionS
  S.single "else"
  IfThenElseEX condition thenValue <$> expressionS


doExpressionS :: ScannerP Expression
doExpressionS = do
  S.singleP "do"
  S.single "do"
  DoEX <$> some doStatementS


doStatementS :: ScannerP DoStatementHskl
doStatementS = asum [
    bindDoStatementS
    , letDoStatementS
    , wrappedDoExpressionS
    , ExpressionST <$> expressionS
    , CommentST <$> S.symbol "comment"
    , unknownStatementAs "haddock"
    , unknownStatementAs "pragma"
    , unknownStatementAs "ERROR"
  ]


bindDoStatementS :: ScannerP DoStatementHskl
bindDoStatementS = do
  S.singleP "bind"
  bindingPattern <- patternS
  S.single "<-"
  BindST . BindContent MonadicBO bindingPattern <$> expressionS


letDoStatementS :: ScannerP DoStatementHskl
letDoStatementS = do
  S.singleP "let"
  _ <- optional $ S.single "let"
  LetShortST <$> localBindingsS


wrappedDoExpressionS :: ScannerP DoStatementHskl
wrappedDoExpressionS = do
  S.singleP "exp"
  ExpressionST <$> expressionS


unknownStatementAs :: String -> ScannerP DoStatementHskl
unknownStatementAs nodeName = do
  ne <- S.single nodeName
  pure $ UnknownST nodeName (spanNE ne)


letInExpressionS :: ScannerP Expression
letInExpressionS = do
  S.singleP "let_in"
  S.single "let"
  bindings <- localBindingsS
  S.single "in"
  LetInEX bindings <$> expressionS


leftSectionS :: ScannerP Expression
leftSectionS = do
  singlePAny [ "left_section", "section_left" ]
  S.single "("
  value <- expressionS
  operator <- operatorS
  S.single ")"
  pure $ LeftSectionEX value operator


rightSectionS :: ScannerP Expression
rightSectionS = do
  singlePAny [ "right_section", "section_right" ]
  S.single "("
  operator <- operatorS
  value <- expressionS
  S.single ")"
  pure $ RightSectionEX operator value


signatureExpressionS :: ScannerP Expression
signatureExpressionS = do
  S.singleP "signature"
  value <- expressionS
  S.single "::"
  SignatureEX value <$> typeSignatureS


negativeExpressionS :: ScannerP Expression
negativeExpressionS = do
  singlePAny [ "negative", "negation" ]
  _ <- optional $ S.single "-"
  NegateEX <$> expressionS


prefixOperatorExpressionS :: ScannerP Expression
prefixOperatorExpressionS = OperatorEX <$> prefixOperatorS


parenExpressionS :: ScannerP Expression
parenExpressionS = do
  S.singleP "parens"
  S.single "("
  value <- expressionS
  S.single ")"
  pure $ ParenEX value


listExpressionS :: ScannerP Expression
listExpressionS = do
  S.singleP "list"
  S.single "["
  values <- expressionS `S.sepBy` S.single ","
  S.single "]"
  pure $ ListEX values


tupleExpressionS :: ScannerP Expression
tupleExpressionS = do
  S.singleP "tuple"
  S.single "("
  values <- expressionS `S.sepBy` S.single ","
  S.single ")"
  pure $ TupleEX values


unitExpressionS :: ScannerP Expression
unitExpressionS = do
  S.singleP "unit"
  S.single "("
  S.single ")"
  pure VoidEX


literalExpressionS :: ScannerP Expression
literalExpressionS = do
  S.singleP "literal"
  LiteralEX <$> literalValueS


quasiquoteExpressionS :: ScannerP Expression
quasiquoteExpressionS = do
  S.singleP "quasiquote"
  _ <- optional $ S.single "["
  quoter <- quoterS
  S.single "|"
  bodyNode <- S.symbol "quasiquote_body"
  S.single "|]"
  pure $ QuasiquoteEX $ Quasiquote quoter bodyNode


quoterS :: ScannerP Identifier
quoterS = do
  S.singleP "quoter"
  qualifiedTermS <|> identifierS


recoverExpressionS :: ScannerP Expression
recoverExpressionS = asum $ map unknownExpressionAs [
    "infix", "apply", "projection", "record", "lambda", "case", "conditional"
    , "do", "let_in", "left_section", "section_left", "right_section"
    , "section_right", "signature", "negative", "negation", "quasiquote"
    , "prefix_id", "prefix_operator", "parens", "list", "tuple", "unit"
    , "literal", "qualified", "constructor", "variable"
    , "arithmetic_sequence", "list_comprehension", "tuple_section"
    , "lambda_case", "multi_way_if", "splice", "quote", "typed_quote"
    , "qualified_do", "recursive_do", "type_application", "ERROR"
  ]


unknownExpressionAs :: String -> ScannerP Expression
unknownExpressionAs nodeName = do
  ne <- S.single nodeName
  pure $ UnknownEX nodeName (spanNE ne)


-- ************* OPERATORS, NAMES AND RECORDS *************

operatorS :: ScannerP Operator
operatorS = asum [
    VariableOP <$> S.symbol "operator"
    , ConstructorOP <$> S.symbol "constructor_operator"
    , namedOperatorS
    , qualifiedOperatorS
  ]


prefixOperatorS :: ScannerP Operator
prefixOperatorS = do
  singlePAny [ "prefix_id", "prefix_operator" ]
  S.single "("
  operator <- operatorS
  S.single ")"
  pure operator


namedOperatorS :: ScannerP Operator
namedOperatorS = do
  S.singleP "infix_id"
  _ <- optional $ S.single "`"
  identifier <- operatorIdentifierS
  _ <- optional $ S.single "`"
  pure $ BackquotedOP identifier


qualifiedOperatorS :: ScannerP Operator
qualifiedOperatorS = do
  S.singleP "qualified"
  moduleName <- qualifiedModuleNameS
  endIdentifier <- asum [
      VarIdent <$> S.symbol "operator"
    , NameIdent <$> S.symbol "constructor_operator"
    ]
  pure $ QualifiedOP $ QualIdent moduleName endIdentifier


operatorIdentifierS :: ScannerP Identifier
operatorIdentifierS = asum [
    identifierS
    , NameIdent <$> S.symbol "constructor"
    ]


qualifiedTermS :: ScannerP Identifier
qualifiedTermS = do
  S.singleP "qualified"
  moduleName <- qualifiedModuleNameS
  endIdentifier <- asum [
      VarIdent <$> S.symbol "variable"
    , NameIdent <$> S.symbol "name"
    , NameIdent <$> S.symbol "constructor"
    ]
  pure $ QualIdent moduleName endIdentifier


fieldNameS :: ScannerP Identifier
fieldNameS = do
  S.singleP "field_name"
  asum [
      VarIdent <$> S.symbol "variable"
    , NameIdent <$> S.symbol "name"
    ]


recordItemsS :: ScannerP a -> ScannerP [a]
recordItemsS itemS = do
  _ <- optional $ S.singleP "fields"
  mbOpen <- optional $ S.single "{"
  items <- itemS `S.sepBy` S.single ","
  when (isJust mbOpen) $ void $ S.single "}"
  pure items


literalValueS :: ScannerP Literal
literalValueS = asum [
    IntegerLT <$> S.symbol "integer"
    , FloatLT <$> S.symbol "float"
    , StringLT <$> S.symbol "string"
    , CharLT <$> S.symbol "char"
  ]


singleAny :: [String] -> ScannerP ()
singleAny nodeNames = void $ asum (map S.single nodeNames)


singlePAny :: [String] -> ScannerP ()
singlePAny nodeNames = void $ asum (map S.singleP nodeNames)