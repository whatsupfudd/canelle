module Cannelle.Haskell.Print where

import Control.Monad (when)

import qualified Data.ByteString as Bs
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Cannelle.TreeSitter.Types (SegmentPos (..))
import Cannelle.TreeSitter.Print (fetchContent, fetchContentBs)

import Cannelle.Haskell.AST

{-
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContent cLines) $ V.zip hsContext.contentDemands (V.fromList [0..])
    identDemands = V.map (T.unpack . T.decodeUtf8 . fetchContentBs cLines) $ V.zip hsContext.contentDemands (V.fromList [0..])
-}

printContext :: Bs.ByteString -> HaskellContext -> IO ()
printContext content hsContext = do
  let
    cLines = V.fromList $ Bs.split 10 content
    indexedDemands = V.indexed hsContext.contentDemands
    demandLines = V.map (\(index, position) -> fetchContent cLines (position, index)) indexedDemands
    identDemands = V.map (\(index, position) ->
      T.unpack . T.decodeUtf8 $ fetchContentBs cLines (position, index)) indexedDemands
  putStrLn "Module Head: "
  printModule identDemands hsContext.moduleDef
  putStrLn "Imports: "
  mapM_ (printImport identDemands) hsContext.imports
  putStrLn "\nDeclarations: "
  V.mapM_ (printDeclaration identDemands) hsContext.declarations
  putStrLn "\nSymbols: "
  V.mapM_ putStr demandLines


printDeclaration demandLines declaration = do
  case declaration of
    SignatureDC names typeAnnotation ->
      putStrLn $ "Signature: " <> showSignatureNames demandLines names
        <> " :: " <> showTypeAnnotation demandLines typeAnnotation
    KindSignatureDC name typeAnnotation ->
      putStrLn $ "Kind: " <> showSignatureName demandLines name
        <> " :: " <> showTypeAnnotation demandLines typeAnnotation
    FunctionDC functionContent ->
      putStrLn $ showFunctionContent demandLines 0 functionContent
    BindingDC binding ->
      putStrLn $ showPatternBinding demandLines 0 binding
    TopSpliceDC expression ->
      putStrLn $ "~ " <> showExpression demandLines 0 expression
    DataDC dataDeclaration ->
      putStrLn $ showDataDeclaration demandLines dataDeclaration
    NewtypeDC newtypeDeclaration ->
      putStrLn $ showNewtypeDeclaration demandLines newtypeDeclaration
    TypeSynonymDC synonymDeclaration ->
      putStrLn $ showTypeSynonymDeclaration demandLines synonymDeclaration
    ClassDC classDeclaration ->
      putStrLn $ showClassDeclaration demandLines classDeclaration
    FamilyDC familyDeclaration ->
      putStrLn $ showFamilyDeclaration demandLines familyDeclaration
    CommentDC ->
      putStrLn "Comment"
    UnknownDC nodeName segmentPos ->
      putStrLn $ "Unknown: " <> nodeName <> " " <> show segmentPos
    other ->
      putStrLn $ show other


showSignatureNames :: V.Vector String -> V.Vector SignatureName -> String
showSignatureNames demandLines =
  L.intercalate ", " . map (showSignatureName demandLines) . V.toList


showSignatureName :: V.Vector String -> SignatureName -> String
showSignatureName demandLines signatureName =
  case signatureName of
    VariableSN anInt -> unrefDemand demandLines anInt
    TypeSN anInt -> unrefDemand demandLines anInt
    ConstructorSN anInt -> unrefDemand demandLines anInt
    OperatorSN anInt -> "(" <> unrefDemand demandLines anInt <> ")"
    ConstructorOperatorSN anInt -> "(" <> unrefDemand demandLines anInt <> ")"


showTypeAnnotation :: V.Vector String -> TypeAnnotation -> String
showTypeAnnotation demandLines typeAnnotation =
  case typeAnnotation of
    NameTA name -> showIdentifier demandLines name
    OperatorTA operator -> showTypeOperator operator
    FunctionTA leftSide rightSide ->
      showTypeAnnotation demandLines leftSide <> " -> " <> showTypeAnnotation demandLines rightSide
    ApplyTA leftSide rightSide ->
      showTypeAnnotation demandLines leftSide <> " " <> showTypeAnnotation demandLines rightSide
    InfixTA leftSide operator rightSide ->
      showTypeAnnotation demandLines leftSide <> " " <> showTypeOperator operator <> " "
        <> showTypeAnnotation demandLines rightSide
    ParenTA innerType -> "(" <> showTypeAnnotation demandLines innerType <> ")"
    VoidTA -> "()"
    ListTA innerType -> "[" <> showTypeAnnotation demandLines innerType <> "]"
    TupleTA innerTypes ->
      "(" <> L.intercalate ", " (map (showTypeAnnotation demandLines) innerTypes) <> ")"
    ContextTA context innerType ->
      showTypeContext demandLines context <> " => " <> showTypeHead demandLines innerType
    ForallTA parameters innerType ->
      "forall " <> unwords (map (showTypeParameter demandLines) parameters) <> ". "
        <> showTypeAnnotation demandLines innerType
    KindTA typeValue typeKind ->
      showTypeAnnotation demandLines typeValue <> " :: " <> showTypeAnnotation demandLines typeKind
    WildcardTA anInt -> "<wildcard " <> show anInt <> ">"
    LiteralTA literal -> show literal
    StrictTA inner -> "!" <> showTypeAnnotation demandLines inner
    UnknownTA nodeName segment ->
      "<unknown-type " <> nodeName <> " " <> show segment <> ">"


showTypeContext :: V.Vector String -> TypeContext -> String
showTypeContext demandLines context =
  case context.constraintsTC of
    [constraint] -> showTypeAnnotation demandLines constraint
    constraints -> "(" <> L.intercalate ", " (map (showTypeAnnotation demandLines) constraints) <> ")"


showTypeParameter :: V.Vector String -> TypeParameter -> String
showTypeParameter demandLines parameter =
  case parameter.kindTP of
    Nothing -> showTypeAnnotation demandLines parameter.parameterTP
    Just typeKind ->
      "(" <> showTypeAnnotation demandLines parameter.parameterTP <> " :: "
        <> showTypeAnnotation demandLines typeKind <> ")"


showTypeOperator :: TypeOperator -> String
showTypeOperator operator =
  case operator of
    OperatorTO anInt -> "<operator " <> show anInt <> ">"
    ConstructorOperatorTO anInt -> "<constructor-operator " <> show anInt <> ">"
    NamedOperatorTO identifier -> "`" <> show identifier <> "`"


showNewtypeDeclaration :: V.Vector String -> NewtypeDeclaration -> String
showNewtypeDeclaration demandLines declaration =
  "newtype " <> showTypeHead demandLines declaration.headNT <> " = "
    <> showDataConstructor demandLines 0 declaration.constructorNT
    <> showDerivingDecls demandLines declaration.derivingNT
    <> showUnknownDeclarations declaration.unknownNT


showTypeSynonymDeclaration :: V.Vector String -> TypeSynonymDeclaration -> String
showTypeSynonymDeclaration demandLines declaration =
  "type " <> showTypeHead demandLines declaration.headTS <> " = "
    <> showTypeAnnotation demandLines declaration.valueTS


showClassDeclaration :: V.Vector String -> ClassDeclaration -> String
showClassDeclaration demandLines declaration =
  "class " <> showTypeHead demandLines declaration.headCL
    <> if V.null declaration.declarationsCL && null declaration.unknownCL then ""
       else " where\n"
         <> spacing 1
         <> L.intercalate ("\n" <> spacing 1)
              (map (showClassMember demandLines) $ V.toList declaration.declarationsCL)
         <> showUnknownDeclarationsAt 1 declaration.unknownCL


showFamilyDeclaration :: V.Vector String -> FamilyDeclaration -> String
showFamilyDeclaration demandLines declaration =
  familyKeyword declaration.familyKindFD <> " family "
    <> showTypeHead demandLines declaration.headFD
    <> showUnknownDeclarations declaration.unknownFD


familyKeyword :: FamilyKind -> String
familyKeyword familyKind =
  case familyKind of
    TypeFK -> "type"
    DataFK -> "data"


showTypeHead :: V.Vector String -> TypeHead -> String
showTypeHead demandLines typeHead =
  showContext typeHead.contextTH
    <> showIdentifier demandLines typeHead.nameTH
    <> showParameters typeHead.parametersTH
    <> showKind typeHead.kindTH
  where
    showContext Nothing = ""
    showContext (Just context) = showTypeContext demandLines context <> " => "

    showParameters [] = ""
    showParameters parameters =
      " " <> unwords (map (showTypeParameter demandLines) parameters)

    showKind Nothing = ""
    showKind (Just typeKind) = " :: " <> showTypeAnnotation demandLines typeKind


showClassMember :: V.Vector String -> Declaration -> String
showClassMember demandLines declaration =
  case declaration of
    SignatureDC names typeAnnotation ->
      showSignatureNames demandLines names <> " :: "
        <> showTypeAnnotation demandLines typeAnnotation

    KindSignatureDC name typeAnnotation ->
      showSignatureName demandLines name <> " :: "
        <> showTypeAnnotation demandLines typeAnnotation

    TypeSynonymDC synonymDeclaration -> showTypeSynonymDeclaration demandLines synonymDeclaration
    FamilyDC familyDeclaration -> showFamilyDeclaration demandLines familyDeclaration
    FunctionDC functionContent -> showFunctionContent demandLines 1 functionContent
    BindingDC binding -> showPatternBinding demandLines 1 binding
    CommentDC -> "-- comment"
    UnknownDC nodeName segment -> "<unknown " <> nodeName <> " " <> show segment <> ">"
    other -> show other


showDerivingDecls :: V.Vector String -> [DerivingDecl] -> String
showDerivingDecls _ [] = ""
showDerivingDecls demandLines derivings =
  "\n" <> spacing 1
    <> L.intercalate ("\n" <> spacing 1)
         (map (showDerivingDecl demandLines 1) derivings)


showUnknownDeclarations :: [Declaration] -> String
showUnknownDeclarations = showUnknownDeclarationsAt 1


showUnknownDeclarationsAt :: Int -> [Declaration] -> String
showUnknownDeclarationsAt _ [] = ""
showUnknownDeclarationsAt level declarations =
  "\n" <> spacing level
    <> L.intercalate ("\n" <> spacing level)
         (map showUnknownDeclaration declarations)


showUnknownDeclaration :: Declaration -> String
showUnknownDeclaration declaration =
  case declaration of
    CommentDC -> "-- comment"
    UnknownDC nodeName segment ->
      "<unknown " <> nodeName <> " " <> show segment <> ">"
    other -> "<recovered " <> show other <> ">"
  

showDataDeclaration :: V.Vector String -> DataDeclaration -> String
showDataDeclaration demandLines dataDeclaration =
  let
    nextLevel = 1
  in
  "data " <> showTypeHead demandLines dataDeclaration.headDC
  <>
  (if null dataDeclaration.constructorsDC then 
    ""
  else
    " = "
    -- <> "\n" <> spacing nextLevel
    <> L.intercalate ("\n" <> spacing nextLevel <> "| ") (map (showDataConstructor demandLines nextLevel) dataDeclaration.constructorsDC)
  )
  <> (if null dataDeclaration.derivingDC then
      ""
    else
      "\n" <> spacing nextLevel <> L.intercalate ("\n" <> spacing nextLevel) (map (showDerivingDecl demandLines nextLevel) dataDeclaration.derivingDC)
  )


showDataConstructor :: V.Vector String -> Int -> DataConstructor -> String
showDataConstructor demandLines level dataConstructor =
  let
    nextLevel = level + 1
  in
  case dataConstructor of
    ClassicCns (name, types) ->
      showIdentifier demandLines name <> " "
      <> L.intercalate (" " <> spacing nextLevel) (map (showTypeAnnotation demandLines) types)
    RecordCns (name, fields) ->
      showIdentifier demandLines name <> " { "
      <> L.intercalate ("\n" <> spacing nextLevel <> ", ") (map (showDataConstructorField demandLines nextLevel) fields) <> " }"
    SumCns sumDecl ->
      showIdentifier demandLines sumDecl.nameCns
      <> if null sumDecl.contentCns then
          ""
        else
          " " <> L.intercalate (" " <> spacing nextLevel) (map (showTypeAnnotation demandLines) sumDecl.contentCns)
    UnknownCns nodeName segment ->
      "<unknown dctor: " <> nodeName <> " " <> show segment <> ">"


showDataConstructorField :: V.Vector String -> Int -> DataConstructorField -> String
showDataConstructorField demandLines level dataConstructorField =
  let
    nextLevel = level + 1
  in
  showIdentifier demandLines dataConstructorField.nameFld <> " :: " <> showTypeAnnotation demandLines dataConstructorField.typeFld


showDerivingDecl :: V.Vector String -> Int -> DerivingDecl -> String
showDerivingDecl demandLines level derivingDecl =
  "deriving" <> showStrategy derivingDecl.strategyDD
    <> showClasses derivingDecl.classesDD
    <> showVia derivingDecl.viaDD
  where
    showStrategy :: Maybe DerivingStrategy -> String
    showStrategy Nothing = ""
    showStrategy (Just StockDS) = " stock"
    showStrategy (Just NewtypeDS) = " newtype"
    showStrategy (Just AnyclassDS) = " anyclass"

    showClasses :: [TypeAnnotation] -> String
    showClasses [] = ""
    showClasses [single] = " " <> showTypeAnnotation demandLines single
    showClasses classes =
      " (" <> L.intercalate ", " (map (showTypeAnnotation demandLines) classes) <> ")"

    showVia Nothing = ""
    showVia (Just viaType) = " via " <> showTypeAnnotation demandLines viaType


showFunctionContent :: V.Vector String -> Int -> FunctionContent -> String
showFunctionContent demandLines level functionContent =
  let
    header = showFunctionName demandLines functionContent.nameFC
      <> showPatternArguments demandLines functionContent.patternsFC
    equations = map (showMatch demandLines level header) functionContent.matchesFC
  in
  L.intercalate ("\n" <> spacing level) equations
    <> showLocalBindings demandLines level functionContent.localBindsFC


showPatternBinding :: V.Vector String -> Int -> PatternBinding -> String
showPatternBinding demandLines level binding =
  let
    header = showPattern demandLines binding.patternBD
    equations = map (showMatch demandLines level header) binding.matchesBD
  in
  L.intercalate ("\n" <> spacing level) equations
    <> showLocalBindings demandLines level binding.localBindsBD


showFunctionName :: V.Vector String -> FunctionName -> String
showFunctionName demandLines functionName =
  case functionName of
    VariableFN anInt -> unrefDemand demandLines anInt
    OperatorFN operator -> "(" <> showOperator demandLines operator <> ")"


showPatternArguments :: V.Vector String -> V.Vector Pattern -> String
showPatternArguments demandLines patterns
  | V.null patterns = ""
  | otherwise = " " <> unwords (map (showPattern demandLines) $ V.toList patterns)


showMatch :: V.Vector String -> Int -> String -> MatchContent -> String
showMatch demandLines _ header match =
  case match.guardsMC of
    [] -> header <> " = " <> showExpression demandLines 0 match.valueMC
    guards -> header <> " | "
      <> L.intercalate ", " (map (showGuard demandLines) guards)
      <> " = " <> showExpression demandLines 0 match.valueMC


showGuard :: V.Vector String -> GuardContent -> String
showGuard demandLines guardContent =
  case guardContent of
    BooleanGuardGC expression -> showExpression demandLines 0 expression
    PatternGuardGC pattern expression ->
      showPattern demandLines pattern <> " <- " <> showExpression demandLines 0 expression
    LetGuardGC bindings ->
      "let " <> L.intercalate "; " (map (showLocalBinding demandLines 0) bindings)
    UnknownGuardGC nodeName segment ->
      "<unknown-guard " <> nodeName <> " " <> show segment <> ">"


showPattern :: V.Vector String -> Pattern -> String
showPattern demandLines pattern =
  case pattern of
    VariablePT anInt -> unrefDemand demandLines anInt
    ConstructorPT identifier -> showIdentifier demandLines identifier
    ApplyPT leftSide rightSide ->
      showPattern demandLines leftSide <> " " <> showPatternAtom demandLines rightSide
    InfixPT leftSide operator rightSide ->
      showPattern demandLines leftSide <> " " <> showOperator demandLines operator
        <> " " <> showPattern demandLines rightSide
    LiteralPT literal -> showLiteral demandLines literal
    NegativePT literal -> "-" <> showLiteral demandLines literal
    WildcardPT _ -> "_"
    ParenPT inner -> "(" <> showPattern demandLines inner <> ")"
    TuplePT patterns ->
      "(" <> L.intercalate ", " (map (showPattern demandLines) patterns) <> ")"
    ListPT patterns ->
      "[" <> L.intercalate ", " (map (showPattern demandLines) patterns) <> "]"
    UnitPT -> "()"
    AsPT anInt inner ->
      unrefDemand demandLines anInt <> "@" <> showPatternAtom demandLines inner
    IrrefutablePT inner -> "~" <> showPatternAtom demandLines inner
    StrictPT inner -> "!" <> showPatternAtom demandLines inner
    RecordPT constructor fields ->
      showPattern demandLines constructor <> " { "
        <> L.intercalate ", " (map (showRecordPatternField demandLines) fields) <> " }"
    ViewPT expression inner ->
      showExpression demandLines 0 expression <> " -> " <> showPattern demandLines inner
    PatternSignaturePT inner annotation ->
      showPattern demandLines inner <> " :: " <> showTypeAnnotation demandLines annotation
    UnknownPT nodeName segment ->
      "<unknown-pattern " <> nodeName <> " " <> show segment <> ">"
    PatternWithComments pattern openingComments trailingComments ->
      L.intercalate "\n" (map (showComment demandLines) openingComments)
        <> showPattern demandLines pattern
        <> L.intercalate "\n" (map (showComment demandLines) trailingComments)


showPatternAtom :: V.Vector String -> Pattern -> String
showPatternAtom demandLines pattern =
  case pattern of
    ApplyPT {} -> "(" <> showPattern demandLines pattern <> ")"
    InfixPT {} -> "(" <> showPattern demandLines pattern <> ")"
    ViewPT {} -> "(" <> showPattern demandLines pattern <> ")"
    PatternSignaturePT {} -> "(" <> showPattern demandLines pattern <> ")"
    _ -> showPattern demandLines pattern


showRecordPatternField :: V.Vector String -> RecordPatternField -> String
showRecordPatternField demandLines field =
  case field of
    RecordWildcardRPF -> ".."
    FieldPatternRPF fieldName Nothing -> showIdentifier demandLines fieldName
    FieldPatternRPF fieldName (Just pattern) ->
      showIdentifier demandLines fieldName <> " = " <> showPattern demandLines pattern


showBindContent :: V.Vector String -> BindContent -> String
showBindContent demandLines binding =
  let operatorText = case binding.operator of
        EquateBO -> "="
        MonadicBO -> "<-"
  in
  showPattern demandLines binding.leftSide <> " " <> operatorText <> " "
    <> showExpression demandLines 0 binding.rightSide


showLocalBindings :: V.Vector String -> Int -> [LocalBinding] -> String
showLocalBindings _ _ [] = ""
showLocalBindings demandLines level bindings =
  "\n" <> spacing level <> "where\n" <> spacing (level + 1)
    <> L.intercalate ("\n" <> spacing (level + 1))
         (map (showLocalBinding demandLines $ level + 1) bindings)


showLocalBinding :: V.Vector String -> Int -> LocalBinding -> String
showLocalBinding demandLines level binding =
  case binding of
    LocalFunctionLB functionContent ->
      showFunctionContent demandLines level functionContent
    LocalPatternLB patternBinding ->
      showPatternBinding demandLines level patternBinding
    LocalSignatureLB names annotation ->
      showSignatureNames demandLines names <> " :: " <> showTypeAnnotation demandLines annotation
    LocalCommentLB _ -> "-- comment"
    LocalUnknownLB nodeName segment ->
      "<unknown-local " <> nodeName <> " " <> show segment <> ">"


showExpression :: V.Vector String -> Int -> Expression -> String
showExpression demandLines level expression =
  let nextLevel = level + 1
  in
  case expression of
    ApplyEX leftSide rightSide ->
      showExpression demandLines level leftSide <> " "
        <> showExpressionAtom demandLines rightSide
    InfixEX leftSide operator rightSide ->
      showExpression demandLines level leftSide <> " " <> showOperator demandLines operator
        <> " " <> showExpression demandLines level rightSide
    LiteralEX literal -> showLiteral demandLines literal
    NegateEX value -> "-" <> showExpressionAtom demandLines value
    DoEX statements ->
      "do\n" <> spacing nextLevel
        <> L.intercalate ("\n" <> spacing nextLevel)
             (map (showStatement demandLines nextLevel) statements)
    CaseEX value alternatives ->
      "case " <> showExpression demandLines 0 value <> " of\n" <> spacing nextLevel
        <> L.intercalate ("\n" <> spacing nextLevel)
             (map (showAlternative demandLines nextLevel) alternatives)
    IfThenElseEX condition thenValue elseValue ->
      "if " <> showExpression demandLines 0 condition
        <> " then " <> showExpression demandLines nextLevel thenValue
        <> " else " <> showExpression demandLines nextLevel elseValue
    LambdaEX patterns value ->
      "\\" <> unwords (map (showPattern demandLines) $ V.toList patterns)
        <> " -> " <> showExpression demandLines level value
    VariableEX anInt -> unrefDemand demandLines anInt
    QualifiedEX identifier -> showIdentifier demandLines identifier
    ProjectionEX prefix field ->
      showExpressionAtom demandLines prefix <> "." <> showIdentifier demandLines field
    LetInEX bindings value ->
      "let\n" <> spacing nextLevel
        <> L.intercalate ("\n" <> spacing nextLevel)
             (map (showLocalBinding demandLines nextLevel) bindings)
        <> "\n" <> spacing level <> "in " <> showExpression demandLines level value
    RecordEX base fields ->
      showExpressionAtom demandLines base <> " { "
        <> L.intercalate ", " (map (showRecordField demandLines) fields) <> " }"
    LeftSectionEX value operator ->
      "(" <> showExpression demandLines 0 value <> " " <> showOperator demandLines operator <> ")"
    RightSectionEX operator value ->
      "(" <> showOperator demandLines operator <> " " <> showExpression demandLines 0 value <> ")"
    SignatureEX value annotation ->
      showExpression demandLines level value <> " :: " <> showTypeAnnotation demandLines annotation
    QuasiquoteEX quasiquote ->
      "[" <> showIdentifier demandLines quasiquote.quoterQQ <> "| "
        <> unrefDemand demandLines quasiquote.bodyQQ <> "|]"
    ConstructorEX anInt -> unrefDemand demandLines anInt
    OperatorEX operator -> "(" <> showOperator demandLines operator <> ")"
    ParenEX value -> "(" <> showExpression demandLines 0 value <> ")"
    ListEX values ->
      "[" <> L.intercalate ", " (map (showExpression demandLines 0) values) <> "]"
    TupleEX values ->
      "(" <> L.intercalate ", " (map (showExpression demandLines 0) values) <> ")"
    VoidEX -> "()"
    UnknownEX nodeName segment ->
      "<unknown-expression " <> nodeName <> " " <> show segment <> ">"
    ExprWithComments expression openingComments trailingComments ->
      L.intercalate ("\n" <> spacing level) (map (showComment demandLines) openingComments)
        <> showExpression demandLines level expression
        <> L.intercalate ("\n" <> spacing level) (map (showComment demandLines) trailingComments)


showExpressionAtom :: V.Vector String -> Expression -> String
showExpressionAtom demandLines expression =
  case expression of
    InfixEX {} -> "(" <> showExpression demandLines 0 expression <> ")"
    LambdaEX {} -> "(" <> showExpression demandLines 0 expression <> ")"
    LetInEX {} -> "(" <> showExpression demandLines 0 expression <> ")"
    SignatureEX {} -> "(" <> showExpression demandLines 0 expression <> ")"
    _ -> showExpression demandLines 0 expression


showOperator :: V.Vector String -> Operator -> String
showOperator demandLines operator =
  case operator of
    VariableOP anInt -> unrefDemand demandLines anInt
    ConstructorOP anInt -> unrefDemand demandLines anInt
    QualifiedOP identifier -> showIdentifier demandLines identifier
    BackquotedOP identifier -> "`" <> showIdentifier demandLines identifier <> "`"


showRecordField :: V.Vector String -> RecordField -> String
showRecordField demandLines field =
  case field.valueRF of
    Nothing -> showIdentifier demandLines field.nameRF
    Just value ->
      showIdentifier demandLines field.nameRF <> " = " <> showExpression demandLines 0 value


showStatement :: V.Vector String -> Int -> DoStatementHskl -> String
showStatement demandLines level statement =
  case statement of
    BindST binding -> showBindContent demandLines binding
    LetShortST bindings ->
      "let\n" <> spacing (level + 1)
        <> L.intercalate ("\n" <> spacing (level + 1))
             (map (showLocalBinding demandLines $ level + 1) bindings)
    ExpressionST expression -> showExpression demandLines level expression
    CommentST _ -> "-- comment"
    UnknownST nodeName segment ->
      "<unknown-statement " <> nodeName <> " " <> show segment <> ">"


showAlternative :: V.Vector String -> Int -> AlternativeCmt -> String
showAlternative demandLines level alternativeCmt =
  case alternativeCmt of
    RealAlternative alternative ->
      showPattern demandLines alternative.patternALT
        <> case alternative.guardedValuesALT of
            [] -> ""
            guardedValues -> L.intercalate ", " (map (showGuardedValue demandLines level) guardedValues)
        <> showLocalBindings demandLines level alternative.localBindsALT
    CommentAlternative anInt -> "-- comment " <> unrefDemand demandLines anInt


showGuardedValue :: V.Vector String -> Int ->([GuardContent], Expression) -> String
showGuardedValue demandLines level (guards, value) =
  (case guards of
    [] -> ""
    guards -> " | " <> L.intercalate ", " (map (showGuard demandLines) guards)
  )
  <> " -> " <> showExpression demandLines (level + 1) value


showLiteral :: V.Vector String -> Literal -> String
showLiteral demandLines literal =
  case literal of
    IntegerLT anInt -> unrefDemand demandLines anInt
    FloatLT anInt -> unrefDemand demandLines anInt
    StringLT anInt -> unrefDemand demandLines anInt
    CharLT anInt -> unrefDemand demandLines anInt


printModule :: V.Vector String -> ModuleDef -> IO ()
printModule demandLines moduleDef = do
  let
    moduleName = case moduleDef.name of
      [] -> "Main"
      names -> L.intercalate "." (map (showIdentifier demandLines) names)
  putStr $ "Module: " <> moduleName
  case moduleDef.exports of
    AllES -> putStrLn ", exports: all"
    OnlyES symbols -> putStrLn $ ", exports: " <> showExposedList demandLines symbols
  mapM_ printUnknown moduleDef.unknownDecls


printImport :: V.Vector String -> Import -> IO ()
printImport demandLines anImport = do
  putStr "- import "
  when anImport.sourceImport $ putStr "{-# SOURCE #-} "
  when anImport.safeImport $ putStr "safe "

  case anImport.qualification of
    PreQualifiedIQ -> putStr "qualified "
    PreAndPostQualifiedIQ -> putStr "qualified "
    _ -> pure ()

  case anImport.packageQualifier of
    Nothing -> pure ()
    Just packageName -> putStr $ unrefDemand demandLines packageName <> " "
  putStr $ L.intercalate "." (map (showIdentifier demandLines) anImport.moduleName)

  case anImport.qualification of
    PostQualifiedIQ -> putStr " qualified"
    PreAndPostQualifiedIQ -> putStr " qualified"
    _ -> pure ()

  case anImport.alias of
    Nothing -> pure ()
    Just alias -> putStr $ " as " <> L.intercalate "." (map (showIdentifier demandLines) alias)

  case anImport.importSpec of
    AllIS -> pure ()
    OnlyIS symbols -> putStr $ " " <> showExposedList demandLines symbols
    HidingIS symbols -> putStr $ " hiding " <> showExposedList demandLines symbols
  putStrLn ""
  mapM_ printUnknown anImport.unknownImportDecls


showExposedList :: V.Vector String -> V.Vector ExposedSymbol -> String
showExposedList demandLines symbols =
  "(" <> L.intercalate ", " (map (printExposedSymbol demandLines) (V.toList symbols)) <> ")"


printExposedSymbol :: V.Vector String -> ExposedSymbol -> String
printExposedSymbol demandLines exposedSymbol =
  case exposedSymbol of
    TypeName anInt -> unrefDemand demandLines anInt
    VarName anInt -> unrefDemand demandLines anInt
    ConstructorName anInt -> unrefDemand demandLines anInt
    OperatorName anInt -> "(" <> unrefDemand demandLines anInt <> ")"
    ConstructorOperatorName anInt -> "(" <> unrefDemand demandLines anInt <> ")"
    ModuleNameEV moduleName ->
      "module " <> L.intercalate "." (map (showIdentifier demandLines) moduleName)
    NamespacedEV TypeNS symbol -> "type " <> printExposedSymbol demandLines symbol
    NamespacedEV PatternNS symbol -> "pattern " <> printExposedSymbol demandLines symbol
    DoubleDotEV -> ".."
    ComplexDef mainSymbol symbols ->
      printExposedSymbol demandLines mainSymbol <> showExposedList demandLines symbols


printUnknown :: Declaration -> IO ()
printUnknown declaration =
  case declaration of
    CommentDC -> putStrLn "  Comment"
    UnknownDC nodeName segment -> putStrLn $ "  Unknown: " <> nodeName <> " " <> show segment
    _ -> putStrLn $ "  Recovered declaration: " <> show declaration


printStatements :: V.Vector DoStatementHskl -> IO ()
printStatements statements = do
  putStrLn $ "Statements: " ++ show statements


showIdentifier :: V.Vector String -> Identifier -> String
showIdentifier demandLines anIdent =
  case anIdent of
    NameIdent anInt -> unrefDemand demandLines anInt
    VarIdent anInt -> unrefDemand demandLines anInt
    QualIdent idents endIdent ->
      unwords (map (showIdentifier demandLines) idents) <> "." <> showIdentifier demandLines endIdent
    DoubleDot -> ".."
    NoOpIdent anInt -> "NoOpIdent " <> unrefDemand demandLines anInt


spacing :: Int -> String
spacing level = replicate (2 * level) ' '

showComment :: V.Vector String -> Int -> String
showComment demandLines anInt =
  "-- cmt: " <> unrefDemand demandLines anInt


unrefDemand :: V.Vector String -> Int -> String
unrefDemand demands index =
  case demands V.!? index of
    Nothing -> "<unknown : " <> show index <> ">"
    Just anIdent -> anIdent