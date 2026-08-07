module Cannelle.Haskell.Parser.Statements where

import Control.Applicative (asum, many, some, (<|>))
import Control.Applicative.Combinators (optional)
import Control.Monad (void)

import Data.Functor (($>))
import Data.List (foldl', singleton)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.TreeSitter.Types (NodeEntry(..))
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Types
import Cannelle.VM.Context (ModuleRepo(modules))
import Cannelle.PHP.Parser.Expressions (nameS)
import Cannelle.React.Transpiler.AnalyzeAst (DescendState(imports))
import Cannelle.Haskell.Parser.Recovery ( unknownDeclS, unhandledAs, spanNE )
import Cannelle.Haskell.Parser.TypeAnnotations (
    contextSignatureS, typeHeadFromAnnotation, typeParametersS, typeSignatureS, typeHeadS, directTypeHeadS
  )
import Cannelle.Haskell.Parser.Computations (functionDeclS, patternBindingS, topSpliceS)


data RootItem =
    ModuleRI ModuleDef
  | ImportsRI (V.Vector Import) (V.Vector Declaration)
  | DeclarationsRI (V.Vector Declaration)
  | UnknownRI Declaration


data ImportItem =
    ImportII Import
  | ImportUnknownII Declaration


data ExposedItem =
    KnownEI ExposedSymbol
  | UnknownEI Declaration


data ConstructorItem =
    ConstructorCI DataConstructor
  | ConstructorUnknownCI Declaration
  | ConstructorSeparatorCI


data DeclarationTailItem =
    DerivingTI DerivingDecl
  | TailUnknownTI Declaration


haskellS :: ScannerP HaskellContext
haskellS = do
  rootItems <- many rootItemS
  let
    initialState = (Nothing, V.empty, V.empty)
    (mbModuleDef, imports, declarations) = foldl' collectRootItem initialState rootItems
  pure HaskellContext {
      moduleDef = fromMaybe implicitModuleDef mbModuleDef
    , imports = imports
    , declarations = declarations
    , contentDemands = V.empty
    }


rootItemS :: ScannerP RootItem
rootItemS =
  asum [
      ModuleRI <$> moduleDeclS
    , do
        (imports, unknowns) <- importListS
        pure $ ImportsRI imports unknowns
    , DeclarationsRI <$> declarationsS
    , UnknownRI <$> unknownDeclS
    ]


collectRootItem :: (Maybe ModuleDef, V.Vector Import, V.Vector Declaration) -> RootItem
    -> (Maybe ModuleDef, V.Vector Import, V.Vector Declaration)
collectRootItem state rootItem =
  case (state, rootItem) of
    ((Nothing, imports, declarations), ModuleRI moduleDef) ->
      (Just moduleDef, imports, declarations)
    ((mbModuleDef, imports, declarations), ModuleRI _) ->
      -- A second header should only occur in malformed input. The first
      -- successfully parsed header remains authoritative.
      (mbModuleDef, imports, declarations)

    ((mbModuleDef, imports, declarations), ImportsRI newImports unknowns) ->
      ( mbModuleDef, imports <> newImports, declarations <> unknowns )

    ((mbModuleDef, imports, declarations), DeclarationsRI newDeclarations) ->
      ( mbModuleDef, imports, declarations <> newDeclarations )

    ((mbModuleDef, imports, declarations), UnknownRI declaration) ->
      ( mbModuleDef, imports, V.snoc declarations declaration )


-- | Temporary representation of a Haskell module with no explicit header.
--
-- An empty module-name path means the implicit Main module. This avoids
-- manufacturing an Int symbol that does not refer to contentDemands.
implicitModuleDef :: ModuleDef
implicitModuleDef =
  ModuleDef {
      name = []
    , exports = AllES
    , unknownDecls = []
    }


moduleDeclS :: ScannerP ModuleDef
moduleDeclS = do
  (moduleName, exportSpec, unknowns) <- debugOpt "md-header" headerS
  pure ModuleDef {
      name = moduleName
    , exports = exportSpec
    , unknownDecls = unknowns
    }


headerS :: ScannerP ( [Identifier], ExportSpec, [Declaration])
headerS = do
  debugOpt "hd-header" $ S.singleP "header"
  debugOpt "hd-module" $ S.single "module"
  moduleName <- debugOpt "hd-moduleId" moduleNameS
  mbExports <- optional $ debugOpt "hd-exports" exportSpecS
  S.single "where"
  let
    (exportSpec, unknowns) = fromMaybe (AllES, []) mbExports
  pure (moduleName, exportSpec, unknowns)


exportSpecS :: ScannerP ( ExportSpec, [Declaration] )
exportSpecS = do
  singlePAny [ "exports", "export_list" ]
  S.single "("
  items <- exposedItemS exportNameS `S.sepBy` S.single ","
  S.single ")"
  let
    (symbols, unknowns) = collectExposedItems items
  pure (OnlyES symbols, unknowns)


exportNameS :: ScannerP ExposedSymbol
exportNameS =
  asum [
      do
        singlePAny [ "export", "export_name" ]
        exportContentS

    , do
        singlePAny [ "module_export" ]
        _ <- optional $ S.single "module"
        ModuleNameEV <$> moduleNameS
    ]


exportContentS :: ScannerP ExposedSymbol
exportContentS =
  asum [
      moduleExportContentS
    , exposedNameContentS
    ]


moduleExportContentS :: ScannerP ExposedSymbol
moduleExportContentS = do
  S.single "module"
  ModuleNameEV <$> moduleNameS


importListS :: ScannerP (V.Vector Import, V.Vector Declaration)
importListS = do
  debugOpt "im-imports" $ S.singleP "imports"
  items <- many importItemS
  pure $ foldl' collectImportItem (V.empty, V.empty) items


importItemS :: ScannerP ImportItem
importItemS =
  asum [
      ImportII <$> importS
    , ImportUnknownII <$> commentS
    , ImportUnknownII <$> unknownDeclS
    ]


collectImportItem :: (V.Vector Import, V.Vector Declaration) -> ImportItem
    -> (V.Vector Import, V.Vector Declaration)
collectImportItem state importItem =
  case (state, importItem) of
    ((imports, unknowns), ImportII importDef) -> (V.snoc imports importDef, unknowns)
    ((imports, unknowns), ImportUnknownII declaration) -> (imports, V.snoc unknowns declaration)


importS :: ScannerP Import
importS = do
  debugOpt "im-import" $ S.singleP "import"
  S.single "import"
  mbSource <- optional $ debugOpt "im-source" sourceImportS
  mbSafe <- optional $ debugOpt "im-safe" $ S.single "safe"
  mbPreQualified <- optional $ debugOpt "im-preQualified" $ S.single "qualified"
  packageQualifier <- optional $ debugOpt "im-package" packageQualifierS
  moduleName <- debugOpt "im-moduleId" moduleNameS
  mbPostQualified <- optional $ debugOpt "im-postQualified" $ S.single "qualified"
  alias <- optional $ debugOpt "im-alias" $ S.single "as" *> moduleNameS
  mbImportSpec <- optional $ debugOpt "im-selection" importSelectionS
  trailingUnknowns <- many importTrailingUnknownS

  let
    qualification = importQualification (isJust mbPreQualified) (isJust mbPostQualified)
    (selection, selectionUnknowns) = fromMaybe (AllIS, []) mbImportSpec

  pure Import {
      moduleName = moduleName
    , packageQualifier = packageQualifier
    , qualification = qualification
    , alias = alias
    , importSpec = selection
    , safeImport = isJust mbSafe
    , sourceImport = isJust mbSource
    , unknownImportDecls =
        selectionUnknowns <> trailingUnknowns
    }


importQualification :: Bool -> Bool -> ImportQualification
importQualification preQualified postQualified =
  case (preQualified, postQualified) of
    (False, False) -> UnqualifiedIQ
    (True, False) -> PreQualifiedIQ
    (False, True) -> PostQualifiedIQ
    (True, True) -> PreAndPostQualifiedIQ


sourceImportS :: ScannerP ()
sourceImportS =
  void $ asum [
      S.single "source"
    , S.single "SOURCE"
    , S.single "pragma"
    ]


packageQualifierS :: ScannerP Int
packageQualifierS =
  asum [
      S.symbol "string"
    , do
        singlePAny [ "import_package", "package", "package_qualifier" ]
        -- TODO: update TreeSitter.Haskell to be able to handle this syntax.
        S.symbol "string"
    ]


importTrailingUnknownS :: ScannerP Declaration
importTrailingUnknownS =
  asum [
      commentS
    , unhandledAs "haddock"
    , unhandledAs "ERROR"
    ]


importSelectionS :: ScannerP ( ImportSpec, [Declaration] )
importSelectionS =
  asum [
      hidingBeforeListS
    , importListSelectionS
    ]


hidingBeforeListS :: ScannerP ( ImportSpec, [Declaration] )
hidingBeforeListS = do
  S.single "hiding"
  singlePAny [
      "import_list"
    , "import_spec"
    ]
  importSelectionBodyS HidingIS


importListSelectionS :: ScannerP ( ImportSpec, [Declaration] )
importListSelectionS = debugOpt "is-importListSelection" $ do
  singlePAny [ "import_list", "import_spec" ]
  mbHiding <- optional $ S.single "hiding"
  let
    constructor = if isJust mbHiding then HidingIS else OnlyIS
  importSelectionBodyS constructor


importSelectionBodyS :: (V.Vector ExposedSymbol -> ImportSpec) -> ScannerP ( ImportSpec, [Declaration] )
importSelectionBodyS constructor = do
  S.single "("
  items <- optional $ exposedItemS importNameS `S.sepBy` S.single ","
  S.single ")"
  let
    (symbols, unknowns) = maybe (V.empty, []) collectExposedItems items
  pure (constructor symbols, unknowns)


importNameS :: ScannerP ExposedSymbol
importNameS = do
  debugOpt "in-importName" $ singlePAny [ "import_name", "import_item" ]
  exposedNameContentS


exposedNameContentS :: ScannerP ExposedSymbol
exposedNameContentS = do
  mbNamespace <- optional namespaceS
  mainName <- exposedMainS
  children <- optional exposedChildrenS

  let
    completeName = case children of
      Nothing -> mainName
      Just childNames -> ComplexDef mainName childNames

  pure $ case mbNamespace of
    Nothing -> completeName
    Just namespace -> NamespacedEV namespace completeName


namespaceS :: ScannerP SymbolNamespace
namespaceS =
  namespaceTokenS <|> do
    singlePAny [ "namespace", "explicit_namespace" ]
    namespaceTokenS


namespaceTokenS :: ScannerP SymbolNamespace
namespaceTokenS =
  asum [
      S.single "type" $> TypeNS
    , S.single "pattern" $> PatternNS
    ]


exposedMainS :: ScannerP ExposedSymbol
exposedMainS =
  asum [
      prefixExposedSymbolS
    , TypeName <$> S.symbol "name"
    , VarName <$> S.symbol "variable"
    , ConstructorName <$> S.symbol "constructor"
    , OperatorName <$> S.symbol "operator"
    , ConstructorOperatorName
        <$> S.symbol "constructor_operator"
    ]


prefixExposedSymbolS :: ScannerP ExposedSymbol
prefixExposedSymbolS = do
  singlePAny [ "prefix_id", "prefix_operator" ]
  S.single "("
  symbol <- asum [
      OperatorName <$> S.symbol "operator"
    , ConstructorOperatorName
        <$> S.symbol "constructor_operator"
    , VarName <$> S.symbol "variable"
    , ConstructorName <$> S.symbol "constructor"
    ]
  S.single ")"
  pure symbol


exposedChildrenS :: ScannerP (V.Vector ExposedSymbol)
exposedChildrenS = do
  debugOpt "in-children" $ S.singleP "children"
  S.single "("
  symbols <- asum [
      S.single "all_names" $> V.singleton DoubleDotEV
    , V.fromList <$> exposedChildS `S.sepBy` S.single ","
    ]
  S.single ")"
  pure symbols


exposedChildS :: ScannerP ExposedSymbol
exposedChildS = do
  mbNamespace <- optional namespaceS
  symbol <- exposedMainS
  pure $ case mbNamespace of
    Nothing -> symbol
    Just namespace -> NamespacedEV namespace symbol


exposedItemS :: ScannerP ExposedSymbol -> ScannerP ExposedItem
exposedItemS itemS =
  asum [
      KnownEI <$> itemS
    , UnknownEI <$> commentS
    , UnknownEI <$> unknownDeclS
    ]


collectExposedItems :: [ExposedItem] -> (V.Vector ExposedSymbol, [Declaration])
collectExposedItems = foldl' collectExposedItem (V.empty, [])


collectExposedItem :: (V.Vector ExposedSymbol, [Declaration]) -> ExposedItem -> (V.Vector ExposedSymbol, [Declaration])
collectExposedItem state item =
  case (state, item) of
    ((symbols, unknowns), KnownEI symbol) -> (V.snoc symbols symbol, unknowns)
    ((symbols, unknowns), UnknownEI declaration) -> (symbols, unknowns <> [declaration])


singlePAny :: [String] -> ScannerP ()
singlePAny nodeNames = void $ asum (map S.singleP nodeNames)

-- ************* DECLARATIONS *************

declarationsS :: ScannerP (V.Vector Declaration)
declarationsS = do
  debugOpt "ds-declarations" $ S.singleP "declarations"
  V.fromList <$> many declarationS


declarationS :: ScannerP Declaration
declarationS = asum [
    debugOpt "dcl-kindSign" kindSignatureDeclS
    , debugOpt "dcl-signature" signatureS
    , debugOpt "dcl-dataDecl" $ DataDC <$> dataDeclS
    , debugOpt "dcl-newtypeDecl" $ NewtypeDC <$> newtypeDeclS
    , debugOpt "dcl-typeSynonymDecl" $ TypeSynonymDC <$> typeSynonymDeclS
    , debugOpt "dcl-classDecl" $ ClassDC <$> classDeclS
    , debugOpt "dcl-familyDecl" $ FamilyDC <$> familyDeclS
    , debugOpt "dcl-functionDecl" $ FunctionDC <$> functionDeclS
    , debugOpt "dcl-patternBinding" $ BindingDC <$> patternBindingS
    , debugOpt "dcl-topSplice" $ TopSpliceDC <$> topSpliceS
    , debugOpt "dcl-postImport" $ PostImportDC <$> importS
    , debugOpt "dcl-comment" commentS
    , debugOpt "dcl-unknownDecl" unknownDeclS
  ]


signatureS :: ScannerP Declaration
signatureS = debugOpt "sg-signature" $ do
  S.singleP "signature"
  firstName <- signatureNameS
  additionalNames <- many $ S.single "," *> signatureNameS
  S.single "::"
  SignatureDC (V.fromList $ firstName : additionalNames) <$> typeSignatureS


kindSignatureDeclS :: ScannerP Declaration
kindSignatureDeclS = debugOpt "kg-kindSignature" $ do
  singlePAny [ "kind_signature", "standalone_kind_signature" ]
  _ <- optional $ S.single "type"
  signatureName <- signatureNameS
  S.single "::"
  KindSignatureDC signatureName <$> typeSignatureS


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
  singlePAny [ "prefix_id", "prefix_operator" ]
  S.single "("
  signatureName <- asum [
      OperatorSN <$> S.symbol "operator"
    , ConstructorOperatorSN <$> S.symbol "constructor_operator"
    , VariableSN <$> S.symbol "variable"
    , ConstructorSN <$> S.symbol "constructor"
    ]
  S.single ")"
  pure signatureName


dataDeclS :: ScannerP DataDeclaration
dataDeclS = debugOpt "dataDecl-top" $ do
  debugOpt "dd-dataType" $ S.singleP "data_type"
  S.single "data"
  typeHead <- typeHeadS
  debugOpt ("dd-typeHead: " <> show typeHead) $ pure ()
  _ <- optional $ S.single "="
  (constructors, constructorUnknowns) <- fromMaybe ([], []) <$> optional dataConstructorsS
  debugOpt ("dd-ctorUnk: " <> show constructorUnknowns) $ pure ()
  (derivings, tailUnknowns) <- collectDeclarationTail <$> many declarationTailItemS
  debugOpt ("dd-tailUnk: " <> show tailUnknowns) $ pure ()
  pure $ DataDeclaration typeHead constructors derivings (constructorUnknowns <> tailUnknowns)


dataConstructorsS :: ScannerP ([DataConstructor], [Declaration])
dataConstructorsS = debugOpt "dc-dataCtor-try" $ do
  debugOpt "dc-dataCtorN" $ S.singleP "data_constructors"
  collectConstructorItems <$> many constructorItemS


constructorItemS :: ScannerP ConstructorItem
constructorItemS = asum [
    ConstructorCI <$> dataConstructorS
    , ConstructorSeparatorCI <$ S.single "|"
    , ConstructorUnknownCI <$> commentS
    -- , ConstructorUnknownCI <$> unknownDeclS
  ]


collectConstructorItems :: [ConstructorItem] -> ([DataConstructor], [Declaration])
collectConstructorItems = foldl' collectConstructorItem ([], [])


collectConstructorItem :: ([DataConstructor], [Declaration]) -> ConstructorItem
    -> ([DataConstructor], [Declaration])
collectConstructorItem state item =
  case (state, item) of
    ((constructors, unknowns), ConstructorCI constructor) -> (constructors <> [constructor], unknowns)
    ((constructors, unknowns), ConstructorUnknownCI unknown) -> (constructors, unknowns <> [unknown])
    (stateValue, ConstructorSeparatorCI) -> stateValue


dataConstructorS :: ScannerP DataConstructor
dataConstructorS = debugOpt "dc-knownDC" $ knownDataConstructorS <|> debugOpt "dc-unknownDC" unknownDataConstructorS


knownDataConstructorS :: ScannerP DataConstructor
knownDataConstructorS = debugOpt "dc-knownDC-try" $ do
  debugOpt "dc-dataCtor1" $ S.singleP "data_constructor"
  constructor <- asum [
    debugOpt "dc-classicCns" $ ClassicCns <$> classicConstructorS
    , debugOpt "dc-recordCns" $ RecordCns <$> recordConstructorS
    , debugOpt "dc-sumCns" $ SumCns <$> sumTypeConstructorS
    ]
  _ <- optional commentS
  pure constructor


unknownDataConstructorS :: ScannerP DataConstructor
unknownDataConstructorS = debugOpt "dc-unknownDC" $ do
  ne <- S.single "data_constructor"
  pure $ UnknownCns ne.name (spanNE ne)


classicConstructorS :: ScannerP (Identifier, [TypeAnnotation])
classicConstructorS = do
  debugOpt "cc-prefix" $ S.singleP "prefix"
  constructorName <- NameIdent <$> S.symbol "constructor"
  types <- many typeSignatureS
  pure (constructorName, types)


newtypeRecordConstructorS :: ScannerP (Identifier, [DataConstructorField])
newtypeRecordConstructorS = do
  constructorName <- debugOpt "rc-constructor" $ NameIdent <$> S.symbol "constructor"
  S.singleP "record"
  debugOpt "rc-field1-inner" $ S.single "{"
  field <- dataConstructorFieldS
  S.single "}"      
  pure (constructorName, [field])


recordConstructorS :: ScannerP (Identifier, [DataConstructorField])
recordConstructorS = do
  S.singleP "record"
  constructorName <- debugOpt "rc-constructor" $ NameIdent <$> S.symbol "constructor"
  S.singleP "fields"
  debugOpt "rc-fieldN-inner" $ S.single "{"
  fields <- dataConstructorFieldS `S.sepBy` S.single ","
  S.single "}"
  pure (constructorName, fields)


dataConstructorFieldS :: ScannerP DataConstructorField
dataConstructorFieldS = do
  S.singleP "field"
  S.singleP "field_name"
  fieldName <- debugOpt "dcf-fieldName" $ VarIdent <$> S.symbol "variable"
  S.single "::"
  DataConstructorField fieldName <$> typeSignatureS


sumTypeConstructorS :: ScannerP SumDecl
sumTypeConstructorS = do
  debugOpt "st-prefix" $ S.singleP "prefix"
  constructorName <- NameIdent <$> S.symbol "constructor"
  SumDecl constructorName <$> many typeSignatureS


newtypeDeclS :: ScannerP NewtypeDeclaration
newtypeDeclS = do
  debugOpt "nt-newtypeDecl" $ singlePAny [ "newtype", "newtype_type" ]
  S.single "newtype"
  typeHead <- typeHeadS
  S.single "="
  constructor <- newtypeConstructorS
  (derivings, unknowns) <- collectDeclarationTail <$> many declarationTailItemS
  pure $ NewtypeDeclaration typeHead constructor derivings unknowns


newtypeConstructorS :: ScannerP DataConstructor
newtypeConstructorS = do
  S.singleP "newtype_constructor"
  asum [
      debugOpt "nt-recordCtor" $ RecordCns <$> newtypeRecordConstructorS
    , debugOpt "nt-sumCtor" $ SumCns <$> sumTypeConstructorS
    , debugOpt "nt-directCtor" $ directNewtypeConstructorS
    ]


directNewtypeConstructorS :: ScannerP DataConstructor
directNewtypeConstructorS = do
  constructorName <- NameIdent <$> S.symbol "constructor"
  fields <- many $ newtypeFieldS <|> typeSignatureS
  pure $ SumCns $ SumDecl constructorName fields


newtypeFieldS :: ScannerP TypeAnnotation
newtypeFieldS = do
  S.singleP "field"
  typeSignatureS


typeSynonymDeclS :: ScannerP TypeSynonymDeclaration
typeSynonymDeclS = do
  debugOpt "sy-typeSynonym" $ S.singleP "type_synomym"
  S.single "type"
  typeHead <- directTypeHeadS
  S.single "="
  TypeSynonymDeclaration typeHead <$> typeSignatureS


derivingS :: ScannerP DerivingDecl
derivingS = do
  debugOpt "dr-deriving" $ S.singleP "deriving"
  S.single "deriving"
  strategy <- optional derivingStrategyS
  viaType <- optional derivingViaS
  classes <- derivingClassesS
  pure $ DerivingDecl strategy classes viaType


derivingStrategyS :: ScannerP DerivingStrategy
derivingStrategyS =
  derivingStrategyTokenS <|> do
    singlePAny [ "strategy", "deriving_strategy" ]
    derivingStrategyTokenS


derivingStrategyTokenS :: ScannerP DerivingStrategy
derivingStrategyTokenS = asum [
    S.single "stock" $> StockDS
    , S.single "newtype" $> NewtypeDS
    , S.single "anyclass" $> AnyclassDS
  ]


derivingViaS :: ScannerP TypeAnnotation
derivingViaS =
  (S.single "via" *> typeSignatureS) <|> do
    S.singleP "via"
    _ <- optional $ S.single "via"
    typeSignatureS


derivingClassesS :: ScannerP [TypeAnnotation]
derivingClassesS =
  do
    S.singleP "tuple"
    S.single "("
    classes <- typeSignatureS `S.sepBy` S.single ","
    S.single ")"
    pure classes
  <|> ((:[]) <$> typeSignatureS)


declarationTailItemS :: ScannerP DeclarationTailItem
declarationTailItemS = asum [
    DerivingTI <$> debugOpt "dti-deriving" derivingS
    -- , TailUnknownTI <$> debugOpt "dti-tailUnknown" unknownDeclS
  ]


collectDeclarationTail :: [DeclarationTailItem] -> ([DerivingDecl], [Declaration])
collectDeclarationTail = foldl' collectDeclarationTailItem ([], [])


collectDeclarationTailItem :: ([DerivingDecl], [Declaration]) -> DeclarationTailItem
    -> ([DerivingDecl], [Declaration])
collectDeclarationTailItem state item =
  case (state, item) of
    ((derivings, unknowns), DerivingTI derivingDecl) -> (derivings <> [derivingDecl], unknowns)
    ((derivings, unknowns), TailUnknownTI unknown) -> (derivings, unknowns <> [unknown])


classDeclS :: ScannerP ClassDeclaration
classDeclS = do
  S.singleP "class"
  debugOpt "cl-class" $ S.single "class"
  typeHead <- typeHeadS
  headUnknowns <- many classHeadUnknownS
  _ <- optional $ S.single "where"
  body <- fromMaybe V.empty <$> optional classDeclarationsS
  pure $ ClassDeclaration typeHead body headUnknowns


classHeadUnknownS :: ScannerP Declaration
classHeadUnknownS = asum [
    commentS
    , unhandledAs "haddock"
    , unhandledAs "functional_dependencies"
    , unhandledAs "fundeps"
    , unhandledAs "pragma"
    , unhandledAs "ERROR"
  ]


classDeclarationsS :: ScannerP (V.Vector Declaration)
classDeclarationsS = do
  singlePAny [ "class_declarations", "class_body" ]
  V.fromList <$> many classDeclarationS


classDeclarationS :: ScannerP Declaration
classDeclarationS = asum [
    debugOpt "cd-kindS" kindSignatureDeclS
    , debugOpt "cd-sign" signatureS
    , debugOpt "cd-typeSyn" $ TypeSynonymDC <$> typeSynonymDeclS
    , debugOpt "cd-family" $ FamilyDC <$> familyDeclS
    , debugOpt "cd-function" $ FunctionDC <$> functionDeclS
    , debugOpt "cd-binding" $ BindingDC <$> patternBindingS
    , commentS
  ]


familyDeclS :: ScannerP FamilyDeclaration
familyDeclS = typeFamilyDeclS <|> dataFamilyDeclS


typeFamilyDeclS :: ScannerP FamilyDeclaration
typeFamilyDeclS = familyDeclOf TypeFK "type_family" "type"


dataFamilyDeclS :: ScannerP FamilyDeclaration
dataFamilyDeclS = familyDeclOf DataFK "data_family" "data"


familyDeclOf :: FamilyKind -> String -> String -> ScannerP FamilyDeclaration
familyDeclOf familyKind nodeName keyword = do
  S.singleP nodeName
  debugOpt ("fm-" <> nodeName) $ S.single keyword
  _ <- optional $ S.single "family"
  typeHead <- typeHeadS
  resultKind <- optional familyResultKindS
  -- unknowns <- many unknownDeclS
  let
    completedHead = case resultKind of
      Nothing -> typeHead
      Just typeKind -> typeHead { kindTH = Just typeKind }
  pure $ FamilyDeclaration familyKind completedHead []


familyResultKindS :: ScannerP TypeAnnotation
familyResultKindS = S.single "::" *> typeSignatureS


commentS :: ScannerP Declaration
commentS = do
  debugOpt "cm-comment" $ S.single "comment"
  pure CommentDC


typeNameS :: ScannerP ExposedSymbol
typeNameS = do
  TypeName <$> S.symbol "name"

varNameS :: ScannerP ExposedSymbol
varNameS = do
  VarName <$> S.symbol "variable"


identifierS :: ScannerP Identifier
identifierS = asum [
  shortIdentS
  , qualifiedNameS
  ]


shortIdentS :: ScannerP Identifier
shortIdentS = asum [
  nameIdentS
  , varIdentS
  ]


nameIdentS :: ScannerP Identifier
nameIdentS = NameIdent <$> S.symbol "name"


varIdentS :: ScannerP Identifier
varIdentS = VarIdent <$> S.symbol "variable"


moduleNameS :: ScannerP [Identifier]
moduleNameS = do
  debugOpt "mn-module" $ S.singleP "module"
  moduleIdS `S.sepBy` S.single "."


moduleIdS :: ScannerP Identifier
moduleIdS = do
  NameIdent <$> S.symbol "module_id"


qualifiedNameS :: ScannerP Identifier
qualifiedNameS = debugOpt "qs-qualified" $ do
  S.singleP "qualified"
  moduleName <- qualifiedModuleNameS
  QualIdent moduleName <$> shortIdentS


qualifiedModuleNameS :: ScannerP [Identifier]
qualifiedModuleNameS = do
  moduleNameS <* S.single "."
