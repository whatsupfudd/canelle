module Cannelle.Haskell.Summary.Print (
    renderModuleSummary, printModuleSummary
  ) where

import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx
import qualified Data.Vector as V

import Cannelle.Haskell.Summary.Types


printModuleSummary :: HaskellModuleSummary -> IO ()
printModuleSummary = Tx.putStr . renderModuleSummary


renderModuleSummary :: HaskellModuleSummary -> Text
renderModuleSummary summary =
  Tx.unlines $ ["module " <> summary.moduleNameMS, renderExports summary.exportsMS]
    <> renderSection "-- *** imports" (map renderImport $ V.toList summary.importsMS)
    <> renderSection "-- *** types" (map renderTypeDefinition $ V.toList summary.typeDefinitionsMS)
    <> renderSection "-- *** kind signatures" (map renderKindSignature $ V.toList summary.kindSignaturesMS)
    <> renderSection "-- *** function signatures" (map renderFunctionSignature $ V.toList summary.functionSignaturesMS)
    <> renderSection "-- *** untyped bindings" (map renderBinding $ V.toList summary.untypedBindingsMS)
    -- <> renderSection "diagnostics" (map renderDiagnostic $ V.toList summary.diagnosticsMS)


renderSection :: Text -> [Text] -> [Text]
renderSection _ [] = []
renderSection title entries = ["", title <> ":"] <> map ("  " <>) entries


renderExports :: ExportSpecSummary -> Text
renderExports exportSpec =
  case exportSpec of
    ExportAllESS -> "exports: all"
    ExportOnlyESS symbols ->
      "exports: " <> Tx.intercalate ", " (map renderSymbol $ V.toList symbols)


renderImport :: ImportSummary -> Text
renderImport importSummary =
  "import " <> sourcePrefix <> safePrefix <> preQualification <> packagePrefix
    <> importSummary.moduleNameIS <> postQualification <> alias
    <> renderImportSelection importSummary.selectionIS
  where
    sourcePrefix = if importSummary.sourceIS then "{-# SOURCE #-} " else ""
    safePrefix = if importSummary.safeIS then "safe " else ""

    preQualification =
      case importSummary.qualificationIS of
        PreQualifiedIQS -> "qualified "
        PreAndPostQualifiedIQS -> "qualified "
        _ -> ""

    postQualification =
      case importSummary.qualificationIS of
        PostQualifiedIQS -> " qualified"
        PreAndPostQualifiedIQS -> " qualified"
        _ -> ""

    packagePrefix = maybe "" (<> " ") importSummary.packageQualifierIS
    alias = maybe "" (" as " <>) importSummary.aliasIS


renderImportSelection :: ImportSelectionSummary -> Text
renderImportSelection selection =
  case selection of
    ImportAllISS -> ""
    ImportOnlyISS symbols ->
      " (" <> Tx.intercalate ", " (map renderSymbol $ V.toList symbols) <> ")"
    ImportHidingISS symbols ->
      " hiding (" <> Tx.intercalate ", " (map renderSymbol $ V.toList symbols) <> ")"


renderSymbol :: SymbolSummary -> Text
renderSymbol symbol =
  case symbol of
    ValueSS name -> name
    TypeSS name ExportNoMembersEMS -> name
    TypeSS name ExportAllMembersEMS -> name <> "(..)"
    TypeSS name (ExportSelectedMembersEMS members) ->
      name <> "(" <> Tx.intercalate ", " (V.toList members) <> ")"
    ConstructorSS name -> name
    OperatorSS name -> "(" <> name <> ")"
    PatternSS name -> "pattern " <> name
    ModuleSS name -> "module " <> name
    UnknownSS name -> "<unknown:" <> name <> ">"


renderTypeDefinition :: TypeDefinitionSummary -> Text
renderTypeDefinition typeDefinition =
  keyword <> " " <> typeDefinition.headTDS
    <> maybe "" (" = " <>) typeDefinition.valueTDS
    <> renderConstructors typeDefinition.constructorsTDS
    -- <> visibility typeDefinition.exportedTDS
    <> renderMethods typeDefinition.methodsTDS
    <> renderDerivings typeDefinition.derivingTDS
    <> "\n"
  where
    keyword =
      case typeDefinition.kindTDS of
        DataTDK -> "data"
        NewtypeTDK -> "newtype"
        TypeSynonymTDK -> "type"
        ClassTDK -> "class"
        TypeFamilyTDK -> "type family"
        DataFamilyTDK -> "data family"


renderConstructors :: V.Vector ConstructorSummary -> Text
renderConstructors constructors
  | V.null constructors = ""
  | otherwise = " = " <> Tx.intercalate " | "
      (map renderConstructor $ V.toList constructors)


renderConstructor :: ConstructorSummary -> Text
renderConstructor constructor =
  constructor.nameCS <> arguments <> fields
  where
    arguments
      | V.null constructor.argumentsCS = ""
      | otherwise = " " <> Tx.unwords (V.toList constructor.argumentsCS)

    fields
      | V.null constructor.fieldsCS = ""
      | otherwise = " { " <> Tx.intercalate ", "
          (map renderField $ V.toList constructor.fieldsCS) <> " }"


renderField :: FieldSummary -> Text
renderField field = field.nameFS <> " :: " <> field.typeFS


renderMethods :: V.Vector FunctionSignatureSummary -> Text
renderMethods methods
  | V.null methods = ""
  | otherwise = "\n    " <> Tx.intercalate "\n    "
      (map renderFunctionSignature $ V.toList methods)


renderDerivings :: V.Vector Text -> Text
renderDerivings derivings
  | V.null derivings = ""
  | otherwise = "\n    " <> Tx.intercalate "\n    " (V.toList derivings)


renderKindSignature :: KindSignatureSummary -> Text
renderKindSignature signature =
  signature.nameKSS <> " :: " <> signature.kindKSS
    -- <> visibility signature.exportedKSS


renderFunctionSignature :: FunctionSignatureSummary -> Text
renderFunctionSignature signature =
  scopePrefix <> signature.nameFSS <> " :: " <> signature.signatureFSS
    -- <> visibility signature.exportedFSS
    -- <> implementation
  where
    scopePrefix =
      case signature.scopeFSS of
        TopLevelFSS -> ""
        ClassFSS className -> className <> "."

    implementation
      | signature.implementedFSS =
          " [equations: " <> Tx.pack (show signature.equationCountFSS) <> "]"
      | otherwise = " [signature only]"


renderBinding :: BindingSummary -> Text
renderBinding binding =
  binding.nameBS <> " [untyped, equations: "
    <> Tx.pack (show binding.equationCountBS) <> "]"
    -- <> visibility binding.exportedBS


renderDiagnostic :: SummaryDiagnostic -> Text
renderDiagnostic diagnostic =
  level <> " " <> diagnostic.codeSD <> ": " <> diagnostic.messageSD
    <> maybe "" (" at " <>) diagnostic.locationSD
  where
    level =
      case diagnostic.levelSD of
        WarningSDL -> "warning"
        ErrorSDL -> "error"


visibility :: Bool -> Text
visibility True = " [exported]"
visibility False = " [private]"