{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Cannelle.Haskell.Summary.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)


data HaskellModuleSummary = HaskellModuleSummary {
    moduleNameMS :: Text
    , exportsMS :: ExportSpecSummary
    , importsMS :: V.Vector ImportSummary
    , typeDefinitionsMS :: V.Vector TypeDefinitionSummary
    , kindSignaturesMS :: V.Vector KindSignatureSummary
    , functionSignaturesMS :: V.Vector FunctionSignatureSummary
    , untypedBindingsMS :: V.Vector BindingSummary
    , diagnosticsMS :: V.Vector SummaryDiagnostic
    , summaryCompleteMS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ExportSpecSummary =
    ExportAllESS
  | ExportOnlyESS (V.Vector SymbolSummary)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ExportMembersSummary =
    ExportNoMembersEMS
  | ExportAllMembersEMS
  | ExportSelectedMembersEMS (V.Vector Text)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data SymbolSummary =
    ValueSS Text
  | TypeSS Text ExportMembersSummary
  | ConstructorSS Text
  | OperatorSS Text
  | PatternSS Text
  | ModuleSS Text
  | UnknownSS Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ImportSummary = ImportSummary {
    moduleNameIS :: Text
    , packageQualifierIS :: Maybe Text
    , qualificationIS :: ImportQualificationSummary
    , aliasIS :: Maybe Text
    , selectionIS :: ImportSelectionSummary
    , safeIS :: Bool
    , sourceIS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ImportQualificationSummary =
    UnqualifiedIQS
  | PreQualifiedIQS
  | PostQualifiedIQS
  | PreAndPostQualifiedIQS
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ImportSelectionSummary =
    ImportAllISS
  | ImportOnlyISS (V.Vector SymbolSummary)
  | ImportHidingISS (V.Vector SymbolSummary)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data TypeDefinitionKind =
    DataTDK
  | NewtypeTDK
  | TypeSynonymTDK
  | ClassTDK
  | TypeFamilyTDK
  | DataFamilyTDK
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data TypeDefinitionSummary = TypeDefinitionSummary {
    kindTDS :: TypeDefinitionKind
    , nameTDS :: Text
    , headTDS :: Text
    , kindSignatureTDS :: Maybe Text
    , valueTDS :: Maybe Text
    , constructorsTDS :: V.Vector ConstructorSummary
    , methodsTDS :: V.Vector FunctionSignatureSummary
    , derivingTDS :: V.Vector Text
    , exportedTDS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data ConstructorSummary = ConstructorSummary {
    nameCS :: Text
    , argumentsCS :: V.Vector Text
    , fieldsCS :: V.Vector FieldSummary
    , exportedCS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data FieldSummary = FieldSummary {
    nameFS :: Text
    , typeFS :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data KindSignatureSummary = KindSignatureSummary {
    nameKSS :: Text
    , kindKSS :: Text
    , exportedKSS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data FunctionScopeSummary =
    TopLevelFSS
  | ClassFSS Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data FunctionSignatureSummary = FunctionSignatureSummary {
    nameFSS :: Text
    , signatureFSS :: Text
    , scopeFSS :: FunctionScopeSummary
    , exportedFSS :: Bool
    , implementedFSS :: Bool
    , equationCountFSS :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data BindingSummary = BindingSummary {
    nameBS :: Text
    , equationCountBS :: Int
    , exportedBS :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data SummaryDiagnosticLevel = WarningSDL | ErrorSDL
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data SummaryDiagnostic = SummaryDiagnostic {
    levelSD :: SummaryDiagnosticLevel
    , codeSD :: Text
    , messageSD :: Text
    , locationSD :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)