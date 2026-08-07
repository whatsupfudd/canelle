module Cannelle.Haskell.Summary (
    summarizeModule, encodeModuleSummary
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import Data.List (foldl')
import qualified Data.Map.Strict as Mp
import Data.Maybe (mapMaybe)
import qualified Data.Set as St
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Vector as V

import qualified Cannelle.Haskell.AST as H
import Cannelle.Haskell.Summary.Resolve
import Cannelle.Haskell.Summary.Types


data ExportIndex = ExportIndex {
    allExportsEI :: Bool
    , valuesEI :: St.Set Text
    , typesEI :: Mp.Map Text ExportMembersSummary
    , constructorsEI :: St.Set Text
  }


summarizeModule :: Bs.ByteString -> H.HaskellContext -> HaskellModuleSummary
summarizeModule content context =
  let
    resolver = mkResolver content context
    declarations = V.toList context.declarations
    exports = summarizeExports resolver context.moduleDef.exports
    exportIndex = buildExportIndex exports
    kindSignatures = summarizeKindSignatures resolver exportIndex declarations
    kindMap = Mp.fromList $ map (\item -> (item.nameKSS, item.kindKSS))
      $ V.toList kindSignatures
    typeDefinitions = V.fromList $ mapMaybe
      (summarizeTypeDefinition resolver exportIndex kindMap) declarations
    topSignatures = summarizeScopedSignatures resolver exportIndex
      TopLevelFSS Nothing declarations
    classSignatures = V.concatMap (.methodsTDS) typeDefinitions
    untypedBindings = summarizeUntypedBindings resolver exportIndex declarations
    diagnostics = V.fromList $ collectDiagnostics context
    imports = V.map (summarizeImport resolver) context.imports
  in
  HaskellModuleSummary {
      moduleNameMS = summarizeModuleName resolver context.moduleDef.name
    , exportsMS = exports
    , importsMS = imports
    , typeDefinitionsMS = typeDefinitions
    , kindSignaturesMS = kindSignatures
    , functionSignaturesMS = topSignatures <> classSignatures
    , untypedBindingsMS = untypedBindings
    , diagnosticsMS = diagnostics
    , summaryCompleteMS = V.null diagnostics
    }


encodeModuleSummary :: HaskellModuleSummary -> Bl.ByteString
encodeModuleSummary = Aeson.encode


summarizeModuleName :: Resolver -> [H.Identifier] -> Text
summarizeModuleName _ [] = "Main"
summarizeModuleName resolver identifiers =
  Tx.intercalate "." $ map (resolveIdentifier resolver) identifiers


summarizeExports :: Resolver -> H.ExportSpec -> ExportSpecSummary
summarizeExports resolver exportSpec =
  case exportSpec of
    H.AllES -> ExportAllESS
    H.OnlyES symbols -> ExportOnlyESS $ V.map (resolveExposedSymbol resolver) symbols


summarizeImport :: Resolver -> H.Import -> ImportSummary
summarizeImport resolver importDef =
  ImportSummary {
      moduleNameIS = Tx.intercalate "."
        $ map (resolveIdentifier resolver) importDef.moduleName
    , packageQualifierIS = resolveRef resolver <$> importDef.packageQualifier
    , qualificationIS = summarizeQualification importDef.qualification
    , aliasIS = fmap (Tx.intercalate "." . map (resolveIdentifier resolver))
        importDef.alias
    , selectionIS = summarizeImportSelection resolver importDef.importSpec
    , safeIS = importDef.safeImport
    , sourceIS = importDef.sourceImport
    }


summarizeQualification :: H.ImportQualification -> ImportQualificationSummary
summarizeQualification qualification =
  case qualification of
    H.UnqualifiedIQ -> UnqualifiedIQS
    H.PreQualifiedIQ -> PreQualifiedIQS
    H.PostQualifiedIQ -> PostQualifiedIQS
    H.PreAndPostQualifiedIQ -> PreAndPostQualifiedIQS


summarizeImportSelection :: Resolver -> H.ImportSpec -> ImportSelectionSummary
summarizeImportSelection resolver importSpec =
  case importSpec of
    H.AllIS -> ImportAllISS
    H.OnlyIS symbols -> ImportOnlyISS $ V.map (resolveExposedSymbol resolver) symbols
    H.HidingIS symbols -> ImportHidingISS $ V.map (resolveExposedSymbol resolver) symbols


buildExportIndex :: ExportSpecSummary -> ExportIndex
buildExportIndex ExportAllESS = ExportIndex True St.empty Mp.empty St.empty
buildExportIndex (ExportOnlyESS symbols) =
  V.foldl' insertExport (ExportIndex False St.empty Mp.empty St.empty) symbols


insertExport :: ExportIndex -> SymbolSummary -> ExportIndex
insertExport exportIndex symbol =
  case symbol of
    ValueSS name -> exportIndex { valuesEI = St.insert name exportIndex.valuesEI }
    OperatorSS name -> exportIndex { valuesEI = St.insert name exportIndex.valuesEI }
    PatternSS name -> exportIndex { valuesEI = St.insert name exportIndex.valuesEI }
    ConstructorSS name ->
      exportIndex { constructorsEI = St.insert name exportIndex.constructorsEI }
    TypeSS name members ->
      exportIndex { typesEI = Mp.insert name members exportIndex.typesEI }
    _ -> exportIndex


isValueExported :: ExportIndex -> Text -> Bool
isValueExported exportIndex name =
  exportIndex.allExportsEI || St.member name exportIndex.valuesEI


isTypeExported :: ExportIndex -> Text -> Bool
isTypeExported exportIndex name =
  exportIndex.allExportsEI || Mp.member name exportIndex.typesEI


isMemberExported :: ExportIndex -> Text -> Text -> Bool
isMemberExported exportIndex owner member
  | exportIndex.allExportsEI = True
  | St.member member exportIndex.constructorsEI = True
  | St.member member exportIndex.valuesEI = True
  | otherwise =
      case Mp.lookup owner exportIndex.typesEI of
        Just ExportAllMembersEMS -> True
        Just (ExportSelectedMembersEMS members) -> V.elem member members
        _ -> False


summarizeKindSignatures :: Resolver -> ExportIndex -> [H.Declaration]
    -> V.Vector KindSignatureSummary
summarizeKindSignatures resolver exportIndex declarations =
  V.fromList [
      KindSignatureSummary name (renderTypeAnnotation resolver typeKind)
        (isTypeExported exportIndex name)
    | H.KindSignatureDC signatureName typeKind <- declarations
    , let name = resolveSignatureName resolver signatureName
    ]


summarizeTypeDefinition :: Resolver -> ExportIndex -> Mp.Map Text Text
    -> H.Declaration -> Maybe TypeDefinitionSummary
summarizeTypeDefinition resolver exportIndex kindMap declaration =
  case declaration of
    H.DataDC dataDeclaration ->
      let
        typeHead = dataDeclaration.headDC
        name = resolveIdentifier resolver typeHead.nameTH
      in
      Just $ TypeDefinitionSummary DataTDK name (renderTypeHead resolver typeHead)
        (Mp.lookup name kindMap) Nothing
        (V.fromList $ map (summarizeConstructor resolver exportIndex name)
          dataDeclaration.constructorsDC)
        V.empty
        (V.fromList $ map (renderDerivingDecl resolver) dataDeclaration.derivingDC)
        (isTypeExported exportIndex name)

    H.NewtypeDC newtypeDeclaration ->
      let
        typeHead = newtypeDeclaration.headNT
        name = resolveIdentifier resolver typeHead.nameTH
      in
      Just $ TypeDefinitionSummary NewtypeTDK name (renderTypeHead resolver typeHead)
        (Mp.lookup name kindMap) Nothing
        (V.singleton $ summarizeConstructor resolver exportIndex name
          newtypeDeclaration.constructorNT)
        V.empty
        (V.fromList $ map (renderDerivingDecl resolver) newtypeDeclaration.derivingNT)
        (isTypeExported exportIndex name)

    H.TypeSynonymDC synonymDeclaration ->
      let
        typeHead = synonymDeclaration.headTS
        name = resolveIdentifier resolver typeHead.nameTH
      in
      Just $ TypeDefinitionSummary TypeSynonymTDK name
        (renderTypeHead resolver typeHead) (Mp.lookup name kindMap)
        (Just $ renderTypeAnnotation resolver synonymDeclaration.valueTS)
        V.empty V.empty V.empty $ isTypeExported exportIndex name

    H.ClassDC classDeclaration ->
      let
        typeHead = classDeclaration.headCL
        name = resolveIdentifier resolver typeHead.nameTH
        methods = summarizeScopedSignatures resolver exportIndex
          (ClassFSS name) (Just name) $ V.toList classDeclaration.declarationsCL
      in
      Just $ TypeDefinitionSummary ClassTDK name (renderTypeHead resolver typeHead)
        (Mp.lookup name kindMap) Nothing V.empty methods V.empty
        (isTypeExported exportIndex name)

    H.FamilyDC familyDeclaration ->
      let
        typeHead = familyDeclaration.headFD
        name = resolveIdentifier resolver typeHead.nameTH
        familyKind =
          case familyDeclaration.familyKindFD of
            H.TypeFK -> TypeFamilyTDK
            H.DataFK -> DataFamilyTDK
      in
      Just $ TypeDefinitionSummary familyKind name (renderTypeHead resolver typeHead)
        (Mp.lookup name kindMap) Nothing V.empty V.empty V.empty
        (isTypeExported exportIndex name)

    _ -> Nothing


summarizeConstructor :: Resolver -> ExportIndex -> Text -> H.DataConstructor
    -> ConstructorSummary
summarizeConstructor resolver exportIndex owner constructor =
  case constructor of
    H.ClassicCns (identifier, arguments) ->
      let name = resolveIdentifier resolver identifier
      in ConstructorSummary name
        (V.fromList $ map (renderTypeAnnotation resolver) arguments)
        V.empty $ isMemberExported exportIndex owner name

    H.RecordCns (identifier, fields) ->
      let name = resolveIdentifier resolver identifier
      in ConstructorSummary name V.empty
        (V.fromList $ map (summarizeField resolver) fields)
        $ isMemberExported exportIndex owner name

    H.SumCns sumDeclaration ->
      let name = resolveIdentifier resolver sumDeclaration.nameCns
      in ConstructorSummary name
        (V.fromList $ map (renderTypeAnnotation resolver) sumDeclaration.contentCns)
        V.empty $ isMemberExported exportIndex owner name

    H.UnknownCns nodeName segment ->
      ConstructorSummary
        ("<unknown:" <> Tx.pack nodeName <> "@" <> Tx.pack (show segment) <> ">")
        V.empty V.empty False


summarizeField :: Resolver -> H.DataConstructorField -> FieldSummary
summarizeField resolver field =
  FieldSummary (resolveIdentifier resolver field.nameFld)
    $ renderTypeAnnotation resolver field.typeFld


summarizeScopedSignatures :: Resolver -> ExportIndex -> FunctionScopeSummary
    -> Maybe Text -> [H.Declaration] -> V.Vector FunctionSignatureSummary
summarizeScopedSignatures resolver exportIndex scope owner declarations =
  let
    signatureFacts = concatMap (declarationSignatureFacts resolver) declarations
    bindingFacts = concatMap (declarationBindingFacts resolver) declarations
    (_, bindingMap) = aggregateBindings bindingFacts
    signatureOrder = orderedNub $ map fst signatureFacts
    signatureMap = Mp.fromList signatureFacts
    exported name =
      case owner of
        Nothing -> isValueExported exportIndex name
        Just className -> isValueExported exportIndex name
          || isMemberExported exportIndex className name
    makeSummary name =
      let equationCount = Mp.findWithDefault 0 name bindingMap
      in FunctionSignatureSummary name
        (Mp.findWithDefault "<missing-signature>" name signatureMap)
        scope (exported name) (equationCount > 0) equationCount
  in
  V.fromList $ map makeSummary signatureOrder


summarizeUntypedBindings :: Resolver -> ExportIndex -> [H.Declaration]
    -> V.Vector BindingSummary
summarizeUntypedBindings resolver exportIndex declarations =
  let
    signatureNames = St.fromList $ concatMap
      (map fst . declarationSignatureFacts resolver) declarations
    (bindingOrder, bindingMap) = aggregateBindings
      $ concatMap (declarationBindingFacts resolver) declarations
    untyped = filter (`St.notMember` signatureNames) bindingOrder
  in
  V.fromList [
      BindingSummary name (Mp.findWithDefault 0 name bindingMap)
        (isValueExported exportIndex name)
    | name <- untyped
    ]


declarationSignatureFacts :: Resolver -> H.Declaration -> [(Text, Text)]
declarationSignatureFacts resolver declaration =
  case declaration of
    H.SignatureDC names typeAnnotation ->
      let rendered = renderTypeAnnotation resolver typeAnnotation
      in [(resolveSignatureName resolver name, rendered) | name <- V.toList names]
    _ -> []


declarationBindingFacts :: Resolver -> H.Declaration -> [(Text, Int)]
declarationBindingFacts resolver declaration =
  case declaration of
    H.FunctionDC functionContent ->
      [(resolveFunctionName resolver functionContent.nameFC,
        length functionContent.matchesFC)]

    H.BindingDC patternBinding ->
      [(name, length patternBinding.matchesBD)
        | name <- boundPatternNames resolver patternBinding.patternBD]

    _ -> []


aggregateBindings :: [(Text, Int)] -> ([Text], Mp.Map Text Int)
aggregateBindings = foldl' collect ([], Mp.empty)
  where
    collect (order, counts) (name, count) =
      let nextOrder = if Mp.member name counts then order else order <> [name]
      in (nextOrder, Mp.insertWith (+) name count counts)


orderedNub :: Ord item => [item] -> [item]
orderedNub = reverse . fst . foldl' collect ([], St.empty)
  where
    collect (items, seen) item
      | St.member item seen = (items, seen)
      | otherwise = (item : items, St.insert item seen)


boundPatternNames :: Resolver -> H.Pattern -> [Text]
boundPatternNames resolver pattern =
  orderedNub $
    case pattern of
      H.VariablePT index -> [resolveRef resolver index]
      H.ApplyPT leftSide rightSide ->
        boundPatternNames resolver leftSide <> boundPatternNames resolver rightSide
      H.InfixPT leftSide _ rightSide ->
        boundPatternNames resolver leftSide <> boundPatternNames resolver rightSide
      H.ParenPT inner -> boundPatternNames resolver inner
      H.TuplePT inner -> concatMap (boundPatternNames resolver) inner
      H.ListPT inner -> concatMap (boundPatternNames resolver) inner
      H.AsPT index inner -> resolveRef resolver index : boundPatternNames resolver inner
      H.IrrefutablePT inner -> boundPatternNames resolver inner
      H.StrictPT inner -> boundPatternNames resolver inner
      H.RecordPT _ fields -> concatMap (boundRecordFieldNames resolver) fields
      H.ViewPT _ inner -> boundPatternNames resolver inner
      H.PatternSignaturePT inner _ -> boundPatternNames resolver inner
      _ -> []


boundRecordFieldNames :: Resolver -> H.RecordPatternField -> [Text]
boundRecordFieldNames resolver field =
  case field of
    H.RecordWildcardRPF -> []
    H.FieldPatternRPF identifier Nothing -> [resolveIdentifier resolver identifier]
    H.FieldPatternRPF _ (Just pattern) -> boundPatternNames resolver pattern


collectDiagnostics :: H.HaskellContext -> [SummaryDiagnostic]
collectDiagnostics context =
  concatMap (declarationDiagnostics "module-header") context.moduleDef.unknownDecls
    <> concatMap importDiagnostics (V.toList context.imports)
    <> concatMap (declarationDiagnostics "module") (V.toList context.declarations)


importDiagnostics :: H.Import -> [SummaryDiagnostic]
importDiagnostics importDef =
  concatMap (declarationDiagnostics "import") importDef.unknownImportDecls


declarationDiagnostics :: Text -> H.Declaration -> [SummaryDiagnostic]
declarationDiagnostics scope declaration =
  case declaration of
    H.UnknownDC nodeName segment ->
      [unknownDiagnostic scope nodeName segment]

    H.SignatureDC _ annotation ->
      typeDiagnostics scope annotation

    H.KindSignatureDC _ annotation ->
      typeDiagnostics scope annotation

    H.DataDC declarationData ->
      concatMap (declarationDiagnostics "data") declarationData.unknownDC
        <> typeHeadDiagnostics scope declarationData.headDC
        <> concatMap constructorDiagnostics declarationData.constructorsDC

    H.NewtypeDC declarationNewtype ->
      concatMap (declarationDiagnostics "newtype") declarationNewtype.unknownNT
        <> typeHeadDiagnostics scope declarationNewtype.headNT
        <> constructorDiagnostics declarationNewtype.constructorNT

    H.TypeSynonymDC synonym ->
      typeHeadDiagnostics scope synonym.headTS <> typeDiagnostics scope synonym.valueTS

    H.ClassDC classDeclaration ->
      concatMap (declarationDiagnostics "class") classDeclaration.unknownCL
        <> typeHeadDiagnostics scope classDeclaration.headCL
        <> concatMap (declarationDiagnostics "class-member")
             (V.toList classDeclaration.declarationsCL)

    H.FamilyDC familyDeclaration ->
      concatMap (declarationDiagnostics "family") familyDeclaration.unknownFD
        <> typeHeadDiagnostics scope familyDeclaration.headFD

    _ -> []


constructorDiagnostics :: H.DataConstructor -> [SummaryDiagnostic]
constructorDiagnostics constructor =
  case constructor of
    H.ClassicCns (_, arguments) -> concatMap (typeDiagnostics "constructor") arguments
    H.RecordCns (_, fields) -> concatMap
      (typeDiagnostics "record-field" . (.typeFld)) fields
    H.SumCns declaration -> concatMap
      (typeDiagnostics "constructor") declaration.contentCns
    H.UnknownCns nodeName segment -> [unknownDiagnostic "constructor" nodeName segment]


typeHeadDiagnostics :: Text -> H.TypeHead -> [SummaryDiagnostic]
typeHeadDiagnostics scope typeHead =
  maybe [] (concatMap (typeDiagnostics scope) . (.constraintsTC)) typeHead.contextTH
    <> concatMap parameterDiagnostics typeHead.parametersTH
    <> maybe [] (typeDiagnostics scope) typeHead.kindTH


parameterDiagnostics :: H.TypeParameter -> [SummaryDiagnostic]
parameterDiagnostics parameter =
  typeDiagnostics "type-parameter" parameter.parameterTP
    <> maybe [] (typeDiagnostics "type-parameter-kind") parameter.kindTP


typeDiagnostics :: Text -> H.TypeAnnotation -> [SummaryDiagnostic]
typeDiagnostics scope annotation =
  case annotation of
    H.FunctionTA leftSide rightSide ->
      typeDiagnostics scope leftSide <> typeDiagnostics scope rightSide
    H.ApplyTA leftSide rightSide ->
      typeDiagnostics scope leftSide <> typeDiagnostics scope rightSide
    H.InfixTA leftSide _ rightSide ->
      typeDiagnostics scope leftSide <> typeDiagnostics scope rightSide
    H.ParenTA inner -> typeDiagnostics scope inner
    H.ListTA inner -> typeDiagnostics scope inner
    H.TupleTA inner -> concatMap (typeDiagnostics scope) inner
    H.ContextTA context typeHead ->
      concatMap (typeDiagnostics scope) context.constraintsTC
        <> typeHeadDiagnostics scope typeHead
    H.ForallTA parameters inner ->
      concatMap parameterDiagnostics parameters <> typeDiagnostics scope inner
    H.KindTA typeValue typeKind ->
      typeDiagnostics scope typeValue <> typeDiagnostics scope typeKind
    H.UnknownTA nodeName segment ->
      [unknownDiagnostic scope nodeName segment]
    _ -> []


unknownDiagnostic :: Show position => Text -> String -> position -> SummaryDiagnostic
unknownDiagnostic scope nodeName segment =
  SummaryDiagnostic {
      levelSD = if nodeName == "ERROR" then ErrorSDL else WarningSDL
    , codeSD = if nodeName == "ERROR" then "tree-sitter-error" else "unknown-syntax"
    , messageSD = scope <> ": " <> Tx.pack nodeName
    , locationSD = Just $ Tx.pack $ show segment
    }