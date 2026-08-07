module Cannelle.Haskell.AST where

import qualified Data.Vector as V
import qualified Data.ByteString as Bs

import TreeSitter.Node (TSPoint(..))

import Cannelle.VM.Context (MainText)
import Cannelle.TreeSitter.Types (SegmentPos)
import Cannelle.PHP.AST (PhpStatement(DoST))


-- TODO: define a proper structure for ElmContext.
data HaskellContext = HaskellContext {
    moduleDef :: ModuleDef
    , imports :: V.Vector Import
    , declarations :: V.Vector Declaration
    , contentDemands :: V.Vector SegmentPos
  }
  deriving Show


data ModuleDef = ModuleDef {
    name :: [Identifier]
    , exports :: ExportSpec
    , unknownDecls :: [Declaration]
  }
  deriving Show


data ExportSpec =
    AllES
  | OnlyES (V.Vector ExposedSymbol)
  deriving Show


data Import = Import {
    moduleName :: [Identifier]
    , packageQualifier :: Maybe Int
    , qualification :: ImportQualification
    , alias :: Maybe [Identifier]
    , importSpec :: ImportSpec
    , safeImport :: Bool
    , sourceImport :: Bool
    , unknownImportDecls :: [Declaration]
  }
  deriving Show


data ImportQualification =
    UnqualifiedIQ
  | PreQualifiedIQ
  | PostQualifiedIQ
  | PreAndPostQualifiedIQ
  deriving Show


data ImportSpec =
    AllIS
  | OnlyIS (V.Vector ExposedSymbol)
  | HidingIS (V.Vector ExposedSymbol)
  deriving Show


data Declaration =
    SignatureDC (V.Vector SignatureName) TypeAnnotation
  | KindSignatureDC SignatureName TypeAnnotation
  | FunctionDC FunctionContent
  | BindingDC PatternBinding
  | TopSpliceDC Expression
  | CommentDC
  | DataDC DataDeclaration
  | TypeSynonymDC TypeSynonymDeclaration
  | NewtypeDC NewtypeDeclaration
  | ClassDC ClassDeclaration
  | FamilyDC FamilyDeclaration
  | InstanceDC
  | DefaultDC
  | ForeignDC
  | PostImportDC Import
  | UnknownDC String SegmentPos
  deriving Show


data SignatureName =
    VariableSN Int
  | TypeSN Int
  | ConstructorSN Int
  | OperatorSN Int
  | ConstructorOperatorSN Int
  deriving Show


data TypeHead = TypeHead {
    nameTH :: Identifier
    , parametersTH :: [TypeParameter]
    , contextTH :: Maybe TypeContext
    , kindTH :: Maybe TypeAnnotation
  }
  deriving Show


data TypeParameter = TypeParameter {
    parameterTP :: TypeAnnotation
    , kindTP :: Maybe TypeAnnotation
  }
  deriving Show


data DataDeclaration = DataDeclaration {
    headDC :: TypeHead
    , constructorsDC :: [DataConstructor]
    , derivingDC :: [DerivingDecl]
    , unknownDC :: [Declaration]
  }
  deriving Show


data NewtypeDeclaration = NewtypeDeclaration {
    headNT :: TypeHead
    , constructorNT :: DataConstructor
    , derivingNT :: [DerivingDecl]
    , unknownNT :: [Declaration]
  }
  deriving Show


data TypeSynonymDeclaration = TypeSynonymDeclaration {
    headTS :: TypeHead
    , valueTS :: TypeAnnotation
  }
  deriving Show


data ClassDeclaration = ClassDeclaration {
    headCL :: TypeHead
    , declarationsCL :: V.Vector Declaration
    , unknownCL :: [Declaration]
  }
  deriving Show


data FamilyKind = TypeFK | DataFK
  deriving Show


data FamilyDeclaration = FamilyDeclaration {
    familyKindFD :: FamilyKind
    , headFD :: TypeHead
    , unknownFD :: [Declaration]
  }
  deriving Show


data DerivingStrategy = StockDS | NewtypeDS | AnyclassDS
  deriving Show


data DerivingDecl = DerivingDecl {
    strategyDD :: Maybe DerivingStrategy
    , classesDD :: [TypeAnnotation]
    , viaDD :: Maybe TypeAnnotation
  }
  deriving Show


data DataConstructor =
    ClassicCns (Identifier, [TypeAnnotation])
  | RecordCns (Identifier, [DataConstructorField])
  | SumCns SumDecl
  | UnknownCns String SegmentPos
  deriving Show


data SumDecl = SumDecl {
    nameCns :: Identifier
    , contentCns :: [TypeAnnotation]
  }
  deriving Show


data DataConstructorField = DataConstructorField {
    nameFld :: Identifier
    , typeFld :: TypeAnnotation
  }
  deriving Show



data FunctionName =
    VariableFN Int
  | OperatorFN Operator
  deriving Show


data FunctionContent = FunctionContent {
    nameFC :: FunctionName
    , patternsFC :: V.Vector Pattern
    , matchesFC :: [MatchContent]
    , localBindsFC :: [LocalBinding]
  }
  deriving Show


data PatternBinding = PatternBinding {
    patternBD :: Pattern
    , matchesBD :: [MatchContent]
    , localBindsBD :: [LocalBinding]
  }
  deriving Show


data MatchContent = MatchContent {
    guardsMC :: [GuardContent]
    , valueMC :: Expression
  }
  deriving Show


data GuardContent =
    BooleanGuardGC Expression
  | PatternGuardGC Pattern Expression
  | LetGuardGC [LocalBinding]
  | UnknownGuardGC String SegmentPos
  deriving Show


data Pattern =
    VariablePT Int
  | ConstructorPT Identifier
  | ApplyPT Pattern Pattern
  | InfixPT Pattern Operator Pattern
  | LiteralPT Literal
  | NegativePT Literal
  | WildcardPT Int
  | ParenPT Pattern
  | TuplePT [Pattern]
  | ListPT [Pattern]
  | UnitPT
  | AsPT Int Pattern
  | IrrefutablePT Pattern
  | StrictPT Pattern
  | RecordPT Pattern [RecordPatternField]
  | ViewPT Expression Pattern
  | PatternSignaturePT Pattern TypeAnnotation
  | UnknownPT String SegmentPos
  | PatternWithComments Pattern [Int] [Int]
  deriving Show


data RecordPatternField =
    FieldPatternRPF Identifier (Maybe Pattern)
  | RecordWildcardRPF
  deriving Show


data Operator =
    VariableOP Int
  | ConstructorOP Int
  | QualifiedOP Identifier
  | BackquotedOP Identifier
  deriving Show


data LocalBinding =
    LocalFunctionLB FunctionContent
  | LocalPatternLB PatternBinding
  | LocalSignatureLB (V.Vector SignatureName) TypeAnnotation
  | LocalCommentLB Int
  | LocalUnknownLB String SegmentPos
  deriving Show


data DoStatementHskl =
    BindST BindContent
  | LetShortST [LocalBinding]
  | ExpressionST Expression
  | CommentST Int
  | UnknownST String SegmentPos
  deriving Show


data BindContent = BindContent {
    operator :: BindOperator
    , leftSide :: Pattern
    , rightSide :: Expression
  }
  deriving Show


data BindOperator = EquateBO | MonadicBO
  deriving Show


data AlternativeCmt =
  RealAlternative Alternative
  | CommentAlternative Int
  deriving Show

data Alternative = Alternative {
    patternALT :: Pattern
    , guardedValuesALT :: [([GuardContent], Expression)]
    -- , guardsALT :: [GuardContent]
    -- , valueALT :: Expression
    , localBindsALT :: [LocalBinding]
  }
  deriving Show


data RecordField = RecordField {
    nameRF :: Identifier
    , valueRF :: Maybe Expression
  }
  deriving Show


data Quasiquote = Quasiquote {
    quoterQQ :: Identifier
    , bodyQQ :: Int
  }
  deriving Show


data Expression =
    ApplyEX Expression Expression
  | InfixEX Expression Operator Expression
  | LiteralEX Literal
  | NegateEX Expression
  | DoEX [DoStatementHskl]
  | CaseEX Expression [AlternativeCmt]
  | IfThenElseEX Expression Expression Expression
  | LambdaEX (V.Vector Pattern) Expression
  | VariableEX Int
  | QualifiedEX Identifier
  | ProjectionEX Expression Identifier
  | LetInEX [LocalBinding] Expression
  | RecordEX Expression [RecordField]
  | LeftSectionEX Expression Operator
  | RightSectionEX Operator Expression
  | SignatureEX Expression TypeAnnotation
  | QuasiquoteEX Quasiquote
  | ConstructorEX Int
  | OperatorEX Operator
  | ParenEX Expression
  | ListEX [Expression]
  | TupleEX [Expression]
  | VoidEX
  | UnknownEX String SegmentPos
  | ExprWithComments Expression [Int] [Int]
  deriving Show


{-
data LeftSideFct =
  VarPatLF Int APattern (Maybe APattern)
  | PatternVarLF Pattern ExtraIdentifer Pattern
  | ParenPatLF LeftSideFct APattern (Maybe APattern)
  deriving Show


data Pattern = Pattern {
    base :: LPattern
    , rest :: Maybe (GConOp, Pattern)
  }
  deriving Show

data GConOp =
  ParenGC
  | ListGC
  | CommasGC
  | QConstructorGC
  deriving Show


data LPattern =
  APatternLP APattern
  | NegativeLP NumericType
  | GConLP (V.Vector APattern)
  deriving Show


data APattern =
  VarAP Int (Maybe APattern)
  | GConstructorAP
  | QConstructorAP (V.Vector FPattern)
  | LiteralAP Literal
  | WildcardAP
  | ParenAP Pattern
  | TupleAP (V.Vector Pattern)
  | ListAP (V.Vector Pattern)
  | IrrefutableAP APattern
  deriving Show


data FPattern = FPattern {
    qvar :: Identifier
    , pattern :: Pattern
  }
  deriving Show
-}


data NumericType =
  IntNT
  | FloatNT
  deriving Show


data TypeContext = TypeContext {
    constraintsTC :: [TypeAnnotation]
  }
  deriving Show


data TypeOperator =
    OperatorTO Int
  | ConstructorOperatorTO Int
  | NamedOperatorTO Identifier
  deriving Show


data TypeLiteral =
    IntegerTL Int
  | StringTL Int
  | CharTL Int
  deriving Show


data TypeAnnotation =
    NameTA Identifier
  | OperatorTA TypeOperator
  | FunctionTA TypeAnnotation TypeAnnotation
  | ApplyTA TypeAnnotation TypeAnnotation
  | InfixTA TypeAnnotation TypeOperator TypeAnnotation
  | ParenTA TypeAnnotation
  | VoidTA
  | ListTA TypeAnnotation
  | TupleTA [TypeAnnotation]
  | ContextTA TypeContext TypeHead -- TypeAnnotation
  | ForallTA [TypeParameter] TypeAnnotation
  | KindTA TypeAnnotation TypeAnnotation
  | WildcardTA Int
  | LiteralTA TypeLiteral
  | StrictTA TypeAnnotation
  | UnknownTA String SegmentPos
  deriving Show



data SymbolNamespace =
    TypeNS
  | PatternNS
  deriving Show


data ExposedSymbol =
    TypeName Int
  | VarName Int
  | ConstructorName Int
  | OperatorName Int
  | ConstructorOperatorName Int
  | ModuleNameEV [Identifier]
  | NamespacedEV SymbolNamespace ExposedSymbol
  | DoubleDotEV
  | ComplexDef ExposedSymbol (V.Vector ExposedSymbol)
  deriving Show


data LetShortContent = LetShortContent {
    leftSide :: Expression
    , rightSide :: Expression
  }
  deriving Show


data LetBinding = 
  SimpleLB BindContent
  | FunctionLB FunctionContent
  deriving Show


data Literal =
  IntegerLT Int
  | FloatLT Int
  | StringLT Int
  | CharLT Int
  deriving Show


data Identifier =
  NameIdent Int
  | VarIdent Int
  | QualIdent [Identifier] Identifier
  | DoubleDot
  -- For debugging:
  | NoOpIdent Int
  deriving Show

data ExtraIdentifer =
  NormalId [Identifier]
  | Backquoted [Identifier]
  deriving Show

