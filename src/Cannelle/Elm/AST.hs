module Cannelle.Elm.AST
  {- ( Context(..)
  , emptyCtx

  , Name(..)
  , QName(..)
  , textQName
  , mkName
  , mbTextName

  , ModuleDecl(..)
  , KindMod(..)
  , ImportDecl(..)

  , Exposing(..)
  , ExposeItem(..)
  , CtorExposure(..)

  , Decl(..)
  , AnnotationDecl(..)
  , ValueDecl(..)
  , AliasDecl(..)
  , UnionDecl(..)
  , Ctor(..)
  , PortDecl(..)
  , InfixDecl(..)
  , Assoc(..)

  , TypeExpr(..)
  , TypeField(..)
  , PatternSummary(..)

  , Comment(..)
  , Diagnostic(..)
  , isAliasTD
  , isUnionTD
  , isAnnoTD
  )
  -} where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Cannelle.TreeSitter.Types ( SegmentPos )


data Context = Context
  { moduleCtx :: Maybe ModuleDecl
  , importsCtx :: Vector ImportDecl
  , declsCtx :: Vector Decl
  , commentsCtx :: Vector Comment
  , demandsCtx :: Vector SegmentPos
  , diagsCtx :: Vector Diagnostic
  }
  deriving (Eq, Show)

emptyCtx :: Context
emptyCtx =
  Context
    { moduleCtx = Nothing
    , importsCtx = V.empty
    , declsCtx = V.empty
    , commentsCtx = V.empty
    , demandsCtx = V.empty
    , diagsCtx = V.empty
    }

data Name = Name
  { textName :: Text
  , spanName :: SegmentPos
  , demandName :: Maybe Int
  }
  deriving (Eq, Show)

newtype QName = QName
  { partsQName :: Vector Name
  }
  deriving (Eq, Show)

mkName :: Text -> SegmentPos -> Name
mkName txt sp = Name { 
    textName = txt
  , spanName = sp
  , demandName = Nothing
  }

mbTextName :: Vector ByteString -> Maybe Int -> Text
mbTextName lines aDemand =
  case aDemand of
    Just demand -> case lines V.!? demand of
      Just line -> decodeUtf8 line
      Nothing -> "<unknown: " <> (T.pack . show) demand <> ">"
    Nothing -> "<nothing>"


textQName :: Vector ByteString -> QName -> Text
textQName lines qn =
  let
    dNames = V.map (mbTextName lines . demandName) qn.partsQName
  in
  T.intercalate (T.pack ".") $ V.toList dNames


data ModuleDecl = ModuleDecl {
    kindMod :: KindMod
  , nameMod :: QName
  , exposingMod :: Exposing
  , spanMod :: SegmentPos
  }
  deriving (Eq, Show)

data KindMod =
    NormalKM
  | PortKM
  | EffectKM
  deriving (Eq, Show)

data ImportDecl = ImportDecl {
    moduleImp :: QName
  , aliasImp :: Maybe QName
  , exposingImp :: Maybe Exposing
  , spanImp :: SegmentPos
  }
  deriving (Eq, Show)


data Exposing =
    AllEX SegmentPos
  | ExplicitEX (Vector ExposeItem) SegmentPos
  deriving (Eq, Show)

data ExposeItem =
    ValueEI Name
  | TypeEI Name CtorExposure
  | OperatorEI Name
  deriving (Eq, Show)


getExpItemName :: ExposeItem -> Name
getExpItemName item =
  case item of
    ValueEI name -> name
    TypeEI name _ -> name
    OperatorEI name -> name


data CtorExposure =
    HiddenCE
  | ExposedCE
  deriving (Eq, Show)

data Decl =
    AnnotationD AnnotationDecl
  | ValueD ValueDecl
  | AliasD AliasDecl
  | UnionD UnionDecl
  | PortD PortDecl
  | InfixD InfixDecl
  | UnhandledD Text SegmentPos
  deriving (Eq, Show)

isAliasTD :: Decl -> Bool
isAliasTD (AliasD _) = True
isAliasTD _ = False

isUnionTD :: Decl -> Bool
isUnionTD (UnionD _) = True
isUnionTD _ = False

isAnnoTD :: Decl -> Bool
isAnnoTD (AnnotationD _) = True
isAnnoTD _ = False


data AnnotationDecl = AnnotationDecl {
    nameAnn :: Name
  , exprAnn :: TypeExpr
  , spanAnn :: SegmentPos
  }
  deriving (Eq, Show)

data ValueDecl = ValueDecl {
    nameVal :: Name
  , argsVal :: Vector PatternSummary
  , spanBodyVal :: SegmentPos
  , spanVal :: SegmentPos
  }
  deriving (Eq, Show)

data PatternSummary =
    NamePS Name
  | IgnoredPS SegmentPos
  | TuplePS (Vector PatternSummary) SegmentPos
  | RecordPS (Vector Name) SegmentPos
  | UnhandledPS Text SegmentPos
  deriving (Eq, Show)

data AliasDecl = AliasDecl {
    nameAlias :: Name
  , paramsAlias :: Vector Name
  , exprAlias :: TypeExpr
  , spanAlias :: SegmentPos
  }
  deriving (Eq, Show)

data UnionDecl = UnionDecl
  { nameUnion :: Name
  , paramsUnion :: Vector Name
  , ctorsUnion :: Vector Ctor
  , spanUnion :: SegmentPos
  }
  deriving (Eq, Show)

data Ctor = Ctor
  { nameCtor :: Name
  , argsCtor :: Vector TypeExpr
  , spanCtor :: SegmentPos
  }
  deriving (Eq, Show)

data TypeExpr
  = VarTE Name
  | RefTE QName (Vector TypeExpr)
  | RecordTE (Maybe Name) (Vector TypeField)
  | TupleTE (Vector TypeExpr)
  | FunTE TypeExpr TypeExpr
  | ParenTE TypeExpr
  | UnitTE SegmentPos
  | UnhandledTE Text SegmentPos
  deriving (Eq, Show)

data TypeField = TypeField
  { nameTF :: Name
  , exprTF :: TypeExpr
  , spanTF :: SegmentPos
  }
  deriving (Eq, Show)

data PortDecl = PortDecl
  { namePort :: Name
  , exprPort :: TypeExpr
  , spanPort :: SegmentPos
  }
  deriving (Eq, Show)

data InfixDecl = InfixDecl
  { nameInfix :: Name
  , assocInfix :: Assoc
  , precInfix :: Int
  , spanInfix :: SegmentPos
  }
  deriving (Eq, Show)

data Assoc
  = LeftA
  | RightA
  | NonA
  deriving (Eq, Show)

data Comment
  = LineC Text SegmentPos
  | BlockC Text SegmentPos
  deriving (Eq, Show)

data Diagnostic
  = ParseDiag Text SegmentPos
  | MissingModuleDiag FilePath
  | ExposedNameDiag Text SegmentPos
  | DuplicateNameDiag Text SegmentPos SegmentPos
  | DuplicateModuleDiag SegmentPos
  | MissingSignatureDiag Text SegmentPos
  | UnhandledNodeDiag Text SegmentPos
  | UnsupportedModuleDiag SegmentPos
  deriving (Eq, Show)
