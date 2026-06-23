module Cannelle.Elm.Resolve
  ( resolveApi
  , indexDecls
  , resolveExposing
  , resolveType
  , resolveValue
  , attachAnn
  , keyName
  , displayName
  , DeclIndex(..)
  , SymbolKey(..)
  , ExposurePlan(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Cannelle.TreeSitter.Types (SegmentPos)

import Cannelle.Elm.AST
import Cannelle.Elm.API


data SymbolKey =
    TextSK Text
  | DemandSK Int
  deriving (Eq, Ord, Show)


data DeclIndex = DeclIndex {
    aliasesIdx :: Map SymbolKey AliasDecl
  , unionsIdx :: Map SymbolKey UnionDecl
  , valuesIdx :: Map SymbolKey ValueDecl
  , annsIdx :: Map SymbolKey AnnotationDecl
  , portsIdx :: Map SymbolKey PortDecl
  , declsIdx :: Vector Decl
  , dupesIdx :: Vector Diagnostic
  }
  deriving (Eq, Show)


data ExposurePlan = ExposurePlan {
    typesPlan :: Vector TypeApi
  , valuesPlan :: Vector ValueApi
  , diagsPlan :: Vector Diagnostic
  }
  deriving (Eq, Show)


resolveApi :: FilePath -> Context -> ModuleApi
resolveApi path ctx =
  let
    idx = indexDecls ctx.declsCtx
    exposingRez = case ctx.moduleCtx of
      Nothing -> ExposurePlan { 
            typesPlan = V.empty
          , valuesPlan = V.empty
          , diagsPlan = V.singleton (MissingModuleDiag path)
          }
      Just modDecl -> resolveExposing idx modDecl.exposingMod
    allDiags = V.concat [ ctx.diagsCtx, idx.dupesIdx, exposingRez.diagsPlan ]
  in
  ModuleApi {
      pathApi = path
    , declModApi = ctx.moduleCtx
    , importsApi = ctx.importsCtx
    , exposingApi = fmap (\modDecl -> modDecl.exposingMod) ctx.moduleCtx
    , typesApi = exposingRez.typesPlan
    , valuesApi = exposingRez.valuesPlan
    , demandsApi = ctx.demandsCtx
    , diagsApi = allDiags
    }


indexDecls :: Vector Decl -> DeclIndex
indexDecls decls =
  V.foldl' indexDecl (emptyIdx decls) decls


resolveExposing :: DeclIndex -> Exposing -> ExposurePlan
resolveExposing idx exposingDecl =
  case exposingDecl of
    AllEX _ -> resolveAll idx
    ExplicitEX items _ -> resolveExplicit idx items


resolveType :: DeclIndex -> Name -> CtorExposure -> Either Diagnostic TypeApi
resolveType idx name ctorExp =
  let
    k = keyName name
  in
  case Map.lookup k idx.aliasesIdx of
    Just alias -> case ctorExp of
      HiddenCE -> Right $ typeFromAlias AliasTX alias
      ExposedCE -> Left $ ExposedNameDiag ("alias cannot expose constructors: " <> displayName name) name.spanName
    Nothing -> case Map.lookup k idx.unionsIdx of
      Just union -> Right $ typeFromUnion (case ctorExp of
              HiddenCE -> OpaqueTX
              ExposedCE -> CtorsTX
          ) union
      Nothing -> Left $ ExposedNameDiag (displayName name) name.spanName


resolveValue :: DeclIndex -> Name -> ValueApi
resolveValue idx name =
  let
    k = keyName name
  in
  case Map.lookup k idx.valuesIdx of
    Just val -> valueFromDecl idx val
    Nothing -> case Map.lookup k idx.portsIdx of
      Just portDecl -> valueFromPort idx portDecl
      Nothing ->
        let
          ann = Map.lookup k idx.annsIdx
        in
        ValueApi {
            nameVA = name
          , annVA = ann
          , declVA = Nothing
          , diagsVA = V.singleton $ ExposedNameDiag (displayName name) name.spanName
          }


attachAnn :: DeclIndex -> ValueDecl -> Maybe AnnotationDecl
attachAnn idx val =
  Map.lookup (keyName val.nameVal) idx.annsIdx


keyName :: Name -> SymbolKey
keyName name
  | hasRealText name = TextSK name.textName
  | Just demand <- name.demandName = DemandSK demand
  | not (T.null name.textName) = TextSK name.textName
  | otherwise = TextSK "<anonymous>"


displayName :: Name -> Text
displayName name
  | hasRealText name = name.textName
  | Just demand <- name.demandName = "#" <> T.pack (show demand)
  | not (T.null name.textName) = name.textName
  | otherwise = "<anonymous>"


emptyIdx :: Vector Decl -> DeclIndex
emptyIdx decls = DeclIndex {
      aliasesIdx = Map.empty
    , unionsIdx = Map.empty
    , valuesIdx = Map.empty
    , annsIdx = Map.empty
    , portsIdx = Map.empty
    , declsIdx = decls
    , dupesIdx = V.empty
    }


indexDecl :: DeclIndex -> Decl -> DeclIndex
indexDecl idx decl =
  case decl of
    AliasD alias -> insertAlias alias idx
    UnionD union -> insertUnion union idx
    AnnotationD ann -> insertAnn ann idx
    ValueD val -> insertValue val idx
    PortD portDecl -> insertPort portDecl idx
    InfixD _ -> idx
    UnhandledD _ _ -> idx


insertAlias :: AliasDecl -> DeclIndex -> DeclIndex
insertAlias alias idx =
  let
    k = keyName alias.nameAlias
    nm = displayName alias.nameAlias
  in
  case (Map.lookup k idx.aliasesIdx, Map.lookup k idx.unionsIdx) of
    (Just old, _) -> addDupe nm old.spanAlias alias.spanAlias idx
    (_, Just old) -> addDupe nm old.spanUnion alias.spanAlias idx
    _ -> idx { aliasesIdx = Map.insert k alias idx.aliasesIdx }


insertUnion :: UnionDecl -> DeclIndex -> DeclIndex
insertUnion union idx =
  let
    k = keyName union.nameUnion
    nm = displayName union.nameUnion
  in
  case (Map.lookup k idx.unionsIdx, Map.lookup k idx.aliasesIdx) of
    (Just old, _) -> addDupe nm old.spanUnion union.spanUnion idx
    (_, Just old) -> addDupe nm old.spanAlias union.spanUnion idx
    _ -> idx { unionsIdx = Map.insert k union idx.unionsIdx }


insertValue :: ValueDecl -> DeclIndex -> DeclIndex
insertValue val idx =
  let
    k = keyName val.nameVal
    nm = displayName val.nameVal
  in case (Map.lookup k idx.valuesIdx, Map.lookup k idx.portsIdx) of
    (Just old, _) -> addDupe nm old.spanVal val.spanVal idx
    (_, Just old) -> addDupe nm old.spanPort val.spanVal idx
    _ -> idx { valuesIdx = Map.insert k val idx.valuesIdx }


insertAnn :: AnnotationDecl -> DeclIndex -> DeclIndex
insertAnn ann idx =
  let
    k = keyName ann.nameAnn
    nm = displayName ann.nameAnn
  in case Map.lookup k idx.annsIdx of
        Just old ->
          addDupe nm old.spanAnn ann.spanAnn idx

        Nothing ->
          idx { annsIdx = Map.insert k ann idx.annsIdx }

insertPort :: PortDecl -> DeclIndex -> DeclIndex
insertPort portDecl idx =
  let
    k = keyName portDecl.namePort
    nm = displayName portDecl.namePort
  in case (Map.lookup k idx.portsIdx, Map.lookup k idx.valuesIdx) of
        (Just old, _) ->
          addDupe nm old.spanPort portDecl.spanPort idx

        (_, Just old) ->
          addDupe nm old.spanVal portDecl.spanPort idx

        _ ->
          idx { portsIdx = Map.insert k portDecl idx.portsIdx }

addDupe :: Text -> SegmentPos -> SegmentPos -> DeclIndex -> DeclIndex
addDupe name firstSp secondSp idx =
  idx
    { dupesIdx =
        idx.dupesIdx `V.snoc` DuplicateNameDiag name firstSp secondSp
    }

emptyPlan :: ExposurePlan
emptyPlan =
  ExposurePlan
    { typesPlan = V.empty
    , valuesPlan = V.empty
    , diagsPlan = V.empty
    }

resolveAll :: DeclIndex -> ExposurePlan
resolveAll idx =
  V.foldl' step emptyPlan idx.declsIdx
  where
    step plan decl = case decl of
      AliasD alias
        | isPrimaryAlias idx alias -> addTypePlan (typeFromAlias AllTX alias) plan
      UnionD union
        | isPrimaryUnion idx union -> addTypePlan (typeFromUnion AllTX union) plan
      ValueD val
        | isPrimaryValue idx val -> addValuePlan (valueFromDecl idx val) plan
      PortD portDecl
        | isPrimaryPort idx portDecl -> addValuePlan (valueFromPort idx portDecl) plan
      _ -> plan


resolveExplicit :: DeclIndex -> Vector ExposeItem -> ExposurePlan
resolveExplicit idx items = V.foldl' stepResolve emptyPlan items
  where
  stepResolve plan item = case item of
    TypeEI name ctorExp -> case resolveType idx name ctorExp of
      Left diag -> plan { diagsPlan = plan.diagsPlan `V.snoc` diag }
      Right typeApi -> addTypePlan typeApi plan
    ValueEI name -> addValuePlan (resolveValue idx name) plan
    OperatorEI name -> plan {
        diagsPlan = plan.diagsPlan `V.snoc` ExposedNameDiag ("operator " <> displayName name) name.spanName
      }


addTypePlan :: TypeApi -> ExposurePlan -> ExposurePlan
addTypePlan item plan = plan { typesPlan = plan.typesPlan `V.snoc` item }

addValuePlan :: ValueApi -> ExposurePlan -> ExposurePlan
addValuePlan item plan =
  plan
    { valuesPlan = plan.valuesPlan `V.snoc` item
    , diagsPlan = plan.diagsPlan <> item.diagsVA
    }


typeFromAlias :: TypeExposure -> AliasDecl -> TypeApi
typeFromAlias exposure alias = TypeApi {
    nameTA = alias.nameAlias
  , exposureTA = exposure
  , declTA = AliasTD alias
  , spanDefTA = alias.spanAlias
  }


typeFromUnion :: TypeExposure -> UnionDecl -> TypeApi
typeFromUnion exposure union = TypeApi {
    nameTA = union.nameUnion
  , exposureTA = exposure
  , declTA = UnionTD union
  , spanDefTA = union.spanUnion
  }


valueFromDecl :: DeclIndex -> ValueDecl -> ValueApi
valueFromDecl idx val =
  let
    ann = attachAnn idx val
    missingSigDiags = if isNothing ann then
          V.singleton $ MissingSignatureDiag (displayName val.nameVal) val.spanVal
        else
          V.empty
  in
  ValueApi {
      nameVA = val.nameVal
    , annVA = ann
    , declVA = Just val
    , diagsVA = missingSigDiags
    }


valueFromPort :: DeclIndex -> PortDecl -> ValueApi
valueFromPort idx portDecl =
  let
    ann = attachPortAnn idx portDecl
  in
  ValueApi {
      nameVA = portDecl.namePort
    , annVA = ann
    , declVA = Nothing
    , diagsVA = V.empty
    }


attachPortAnn :: DeclIndex -> PortDecl -> Maybe AnnotationDecl
attachPortAnn idx portDecl =
  Map.lookup (keyName portDecl.namePort) idx.annsIdx


isPrimaryAlias :: DeclIndex -> AliasDecl -> Bool
isPrimaryAlias idx alias =
  case Map.lookup (keyName alias.nameAlias) idx.aliasesIdx of
    Just alias0 -> alias0 == alias
    Nothing -> False


isPrimaryUnion :: DeclIndex -> UnionDecl -> Bool
isPrimaryUnion idx union =
  case Map.lookup (keyName union.nameUnion) idx.unionsIdx of
    Just union0 -> union0 == union
    Nothing -> False


isPrimaryValue :: DeclIndex -> ValueDecl -> Bool
isPrimaryValue idx val =
  case Map.lookup (keyName val.nameVal) idx.valuesIdx of
    Just val0 ->
      val0 == val

    Nothing ->
      False

isPrimaryPort :: DeclIndex -> PortDecl -> Bool
isPrimaryPort idx portDecl =
  case Map.lookup (keyName portDecl.namePort) idx.portsIdx of
    Just port0 ->
      port0 == portDecl

    Nothing ->
      False

hasRealText :: Name -> Bool
hasRealText name =
  case name.demandName of
    Just demand ->
      let demandTxt = T.pack (show demand)
       in not (T.null name.textName)
            && name.textName /= demandTxt
            && name.textName /= ("#" <> demandTxt)

    Nothing ->
      not (T.null name.textName)