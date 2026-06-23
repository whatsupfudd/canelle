module Cannelle.Elm.API
  ( ModuleApi(..)
  , TypeApi(..)
  , ValueApi(..)
  , TypeDecl(..)
  , TypeExposure(..)
  , fromContext
  ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Cannelle.TreeSitter.Types (SegmentPos)

import Cannelle.Elm.AST

data ModuleApi = ModuleApi {
    pathApi :: FilePath
  , declModApi :: Maybe ModuleDecl
  , importsApi :: Vector ImportDecl
  , exposingApi :: Maybe Exposing
  , typesApi :: Vector TypeApi
  , valuesApi :: Vector ValueApi
  , diagsApi :: Vector Diagnostic
  , demandsApi :: Vector SegmentPos
  }
  deriving (Eq, Show)

data TypeApi = TypeApi {
    nameTA :: Name
  , exposureTA :: TypeExposure
  , declTA :: TypeDecl
  , spanDefTA :: SegmentPos
  }
  deriving (Eq, Show)

data TypeDecl =
    AliasTD AliasDecl
  | UnionTD UnionDecl
  deriving (Eq, Show)

data TypeExposure =
    AliasTX
  | OpaqueTX
  | CtorsTX
  | AllTX
  deriving (Eq, Show)

data ValueApi = ValueApi {
    nameVA :: Name
  , annVA :: Maybe AnnotationDecl
  , declVA :: Maybe ValueDecl
  , diagsVA :: Vector Diagnostic
  }
  deriving (Eq, Show)

fromContext :: FilePath -> Context -> ModuleApi
fromContext path ctx =
  let
    typesApi = V.map typeFromDecl $ V.mapMaybe (\x -> if isAliasTD x || isUnionTD x then Just x else Nothing) ctx.declsCtx
    valuesApi = V.map valueFromDecl $ V.mapMaybe (\x -> if isAnnoTD x then Just x else Nothing) ctx.declsCtx
  in
  ModuleApi {
    pathApi = path
  , declModApi = ctx.moduleCtx
  , importsApi = ctx.importsCtx
  , exposingApi = exposingMod <$> ctx.moduleCtx
  , typesApi = typesApi
  , valuesApi = valuesApi
  , diagsApi = ctx.diagsCtx
  , demandsApi = ctx.demandsCtx
  }

{-
data AliasDecl = AliasDecl {
    nameAlias :: Name
  , paramsAlias :: Vector Name
  , exprAlias :: TypeExpr
  , spanAlias :: SegmentPos
  }

data UnionDecl = UnionDecl
  { nameUnion :: Name
  , paramsUnion :: Vector Name
  , ctorsUnion :: Vector Ctor
  , spanUnion :: SegmentPos
  }

data Ctor = Ctor
  { nameCtor :: Name
  , argsCtor :: Vector TypeExpr
  , spanCtor :: SegmentPos
  }

data AnnotationDecl = AnnotationDecl {
    nameAnn :: Name
  , exprAnn :: TypeExpr
  , spanAnn :: SegmentPos
  }
-}

typeFromDecl :: Decl -> TypeApi
typeFromDecl decl =
  case decl of
    AliasD alias ->
      let
        exposure = case alias.exprAlias of
          RecordTE _ _ -> OpaqueTX
          _ -> AllTX
        declarations = AliasTD alias
      in
      TypeApi { nameTA = alias.nameAlias, exposureTA = exposure, declTA = declarations, spanDefTA = alias.spanAlias }
    UnionD union ->
      let
        exposure = case union.ctorsUnion of
          ctors | V.length ctors > 0 -> CtorsTX
          _ -> OpaqueTX
        declarations = UnionTD union
      in
      TypeApi { nameTA = union.nameUnion, exposureTA = exposure, declTA = declarations, spanDefTA = union.spanUnion }
    _ -> error "typeFromDecl: not a type declaration"

valueFromDecl :: Decl -> ValueApi
valueFromDecl decl =
  case decl of
    AnnotationD annotation -> ValueApi { nameVA = annotation.nameAnn, annVA = Just annotation, declVA = Nothing, diagsVA = V.empty }
    _ -> error "valueFromDecl: not a value declaration"