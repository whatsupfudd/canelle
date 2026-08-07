module Cannelle.Haskell.Summary.Resolve (
    Resolver, mkResolver, resolveRef, resolveIdentifier
    , resolveSignatureName, resolveFunctionName, resolveOperatorName
    , resolveExposedSymbol, renderSymbolSummary
    , renderTypeAnnotation, renderTypeHead, renderDerivingDecl
  ) where

import qualified Data.ByteString as Bs
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Cannelle.TreeSitter.Print (fetchContentBs)
import qualified Cannelle.Haskell.AST as H
import Cannelle.Haskell.Summary.Types


data Resolver = Resolver {
    symbolsR :: V.Vector Text
  }


mkResolver :: Bs.ByteString -> H.HaskellContext -> Resolver
mkResolver content context =
  let
    contentLines = V.fromList $ Bs.split 10 content
    indexedPositions = V.indexed context.contentDemands
    resolvePosition (index, position) =
      TE.decodeUtf8 $ fetchContentBs contentLines (position, index)
  in
  Resolver $ V.map resolvePosition indexedPositions


resolveRef :: Resolver -> Int -> Text
resolveRef resolver index =
  fromMaybe ("<unknown:" <> Tx.pack (show index) <> ">") $ resolver.symbolsR V.!? index


resolveIdentifier :: Resolver -> H.Identifier -> Text
resolveIdentifier resolver identifier =
  case identifier of
    H.NameIdent index -> resolveRef resolver index
    H.VarIdent index -> resolveRef resolver index
    H.QualIdent modules endIdentifier ->
      Tx.intercalate "." $ map (resolveIdentifier resolver) modules
        <> [resolveIdentifier resolver endIdentifier]
    H.DoubleDot -> ".."
    H.NoOpIdent index -> "<noop:" <> resolveRef resolver index <> ">"


resolveSignatureName :: Resolver -> H.SignatureName -> Text
resolveSignatureName resolver signatureName =
  case signatureName of
    H.VariableSN index -> resolveRef resolver index
    H.TypeSN index -> resolveRef resolver index
    H.ConstructorSN index -> resolveRef resolver index
    H.OperatorSN index -> resolveRef resolver index
    H.ConstructorOperatorSN index -> resolveRef resolver index


resolveFunctionName :: Resolver -> H.FunctionName -> Text
resolveFunctionName resolver functionName =
  case functionName of
    H.VariableFN index -> resolveRef resolver index
    H.OperatorFN operator -> resolveOperatorName resolver operator


resolveOperatorName :: Resolver -> H.Operator -> Text
resolveOperatorName resolver operator =
  case operator of
    H.VariableOP index -> resolveRef resolver index
    H.ConstructorOP index -> resolveRef resolver index
    H.QualifiedOP identifier -> resolveIdentifier resolver identifier
    H.BackquotedOP identifier -> resolveIdentifier resolver identifier


renderOperator :: Resolver -> H.Operator -> Text
renderOperator resolver operator =
  case operator of
    H.BackquotedOP identifier -> "`" <> resolveIdentifier resolver identifier <> "`"
    _ -> resolveOperatorName resolver operator


renderTypeOperator :: Resolver -> H.TypeOperator -> Text
renderTypeOperator resolver operator =
  case operator of
    H.OperatorTO index -> resolveRef resolver index
    H.ConstructorOperatorTO index -> resolveRef resolver index
    H.NamedOperatorTO identifier -> "`" <> resolveIdentifier resolver identifier <> "`"


renderTypeAnnotation :: Resolver -> H.TypeAnnotation -> Text
renderTypeAnnotation resolver = renderTypePrec resolver 0


renderTypePrec :: Resolver -> Int -> H.TypeAnnotation -> Text
renderTypePrec resolver parentPrecedence annotation =
  let
    (precedence, rendered) =
      case annotation of
        H.NameTA identifier ->
          (3, resolveIdentifier resolver identifier)

        H.OperatorTA operator ->
          (3, "(" <> renderTypeOperator resolver operator <> ")")

        H.FunctionTA leftSide rightSide ->
          (0, renderTypePrec resolver 1 leftSide <> " -> "
            <> renderTypePrec resolver 0 rightSide)

        H.ApplyTA leftSide rightSide ->
          (2, renderTypePrec resolver 2 leftSide <> " "
            <> renderTypePrec resolver 3 rightSide)

        H.InfixTA leftSide operator rightSide ->
          (1, renderTypePrec resolver 2 leftSide <> " "
            <> renderTypeOperator resolver operator <> " "
            <> renderTypePrec resolver 2 rightSide)

        H.ParenTA inner ->
          (3, "(" <> renderTypePrec resolver 0 inner <> ")")

        H.VoidTA ->
          (3, "()")

        H.ListTA inner ->
          (3, "[" <> renderTypePrec resolver 0 inner <> "]")

        H.TupleTA inner ->
          (3, "(" <> Tx.intercalate ", " (map (renderTypePrec resolver 0) inner) <> ")")

        H.ContextTA context typeHead ->
          (0, renderTypeContext resolver context <> " => "
            <> renderTypeHead resolver typeHead)

        H.ForallTA parameters inner ->
          (0, "forall " <> Tx.unwords (map (renderTypeParameter resolver) parameters)
            <> ". " <> renderTypePrec resolver 0 inner)

        H.KindTA typeValue typeKind ->
          (0, renderTypePrec resolver 1 typeValue <> " :: "
            <> renderTypePrec resolver 0 typeKind)

        H.WildcardTA index ->
          (3, resolveRef resolver index)

        H.LiteralTA literal ->
          (3, renderTypeLiteral resolver literal)

        H.StrictTA inner ->
          (3, "!" <> renderTypePrec resolver 0 inner)

        H.UnknownTA nodeName segment ->
          (3, "<unknown-type:" <> Tx.pack nodeName <> "@" <> Tx.pack (show segment) <> ">")
  in
  if precedence < parentPrecedence then "(" <> rendered <> ")" else rendered


renderTypeHead :: Resolver -> H.TypeHead -> Text
renderTypeHead resolver typeHead =
  maybe "" (\context -> renderTypeContext resolver context <> " => ") typeHead.contextTH
    <> resolveIdentifier resolver typeHead.nameTH
    <> renderTypeParameters resolver typeHead.parametersTH
    <> maybe "" (\typeKind -> " :: " <> renderTypeAnnotation resolver typeKind) typeHead.kindTH


renderTypeParameters :: Resolver -> [H.TypeParameter] -> Text
renderTypeParameters _ [] = ""
renderTypeParameters resolver parameters =
  " " <> Tx.unwords (map (renderTypeParameter resolver) parameters)


renderTypeParameter :: Resolver -> H.TypeParameter -> Text
renderTypeParameter resolver parameter =
  case parameter.kindTP of
    Nothing -> renderTypeAnnotation resolver parameter.parameterTP
    Just typeKind -> "(" <> renderTypeAnnotation resolver parameter.parameterTP
      <> " :: " <> renderTypeAnnotation resolver typeKind <> ")"


renderTypeContext :: Resolver -> H.TypeContext -> Text
renderTypeContext resolver context =
  case context.constraintsTC of
    [constraint] -> renderTypeAnnotation resolver constraint
    constraints -> "("
      <> Tx.intercalate ", " (map (renderTypeAnnotation resolver) constraints)
      <> ")"


renderTypeLiteral :: Resolver -> H.TypeLiteral -> Text
renderTypeLiteral resolver literal =
  case literal of
    H.IntegerTL index -> resolveRef resolver index
    H.StringTL index -> resolveRef resolver index
    H.CharTL index -> resolveRef resolver index


renderDerivingDecl :: Resolver -> H.DerivingDecl -> Text
renderDerivingDecl resolver derivingDecl =
  "deriving" <> strategy <> classes <> viaType
  where
    strategy =
      case derivingDecl.strategyDD of
        Nothing -> ""
        Just H.StockDS -> " stock"
        Just H.NewtypeDS -> " newtype"
        Just H.AnyclassDS -> " anyclass"

    classes =
      case derivingDecl.classesDD of
        [] -> ""
        [single] -> " " <> renderTypeAnnotation resolver single
        manyClasses -> " (" <> Tx.intercalate ", "
          (map (renderTypeAnnotation resolver) manyClasses) <> ")"

    viaType = maybe "" (\annotation ->
      " via " <> renderTypeAnnotation resolver annotation) derivingDecl.viaDD


resolveExposedSymbol :: Resolver -> H.ExposedSymbol -> SymbolSummary
resolveExposedSymbol resolver exposedSymbol =
  case exposedSymbol of
    H.TypeName index ->
      TypeSS (resolveRef resolver index) ExportNoMembersEMS

    H.VarName index ->
      ValueSS $ resolveRef resolver index

    H.ConstructorName index ->
      ConstructorSS $ resolveRef resolver index

    H.OperatorName index ->
      OperatorSS $ resolveRef resolver index

    H.ConstructorOperatorName index ->
      OperatorSS $ resolveRef resolver index

    H.ModuleNameEV identifiers ->
      ModuleSS $ Tx.intercalate "." $ map (resolveIdentifier resolver) identifiers

    H.NamespacedEV H.TypeNS symbol ->
      let resolved = resolveExposedSymbol resolver symbol
      in TypeSS (symbolName resolved) $ symbolMembers resolved

    H.NamespacedEV H.PatternNS symbol ->
      PatternSS $ symbolName $ resolveExposedSymbol resolver symbol

    H.DoubleDotEV ->
      UnknownSS ".."

    H.ComplexDef symbol children ->
      let
        resolved = resolveExposedSymbol resolver symbol
        members
          | V.any isDoubleDot children = ExportAllMembersEMS
          | otherwise = ExportSelectedMembersEMS $ V.map
              (symbolName . resolveExposedSymbol resolver) children
      in
      case resolved of
        TypeSS name _ -> TypeSS name members
        _ -> UnknownSS $ renderSymbolSummary resolved
          <> "(" <> renderMembers members <> ")"


isDoubleDot :: H.ExposedSymbol -> Bool
isDoubleDot H.DoubleDotEV = True
isDoubleDot _ = False


symbolName :: SymbolSummary -> Text
symbolName symbol =
  case symbol of
    ValueSS name -> name
    TypeSS name _ -> name
    ConstructorSS name -> name
    OperatorSS name -> name
    PatternSS name -> name
    ModuleSS name -> name
    UnknownSS name -> name


symbolMembers :: SymbolSummary -> ExportMembersSummary
symbolMembers symbol =
  case symbol of
    TypeSS _ members -> members
    _ -> ExportNoMembersEMS


renderSymbolSummary :: SymbolSummary -> Text
renderSymbolSummary symbol =
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


renderMembers :: ExportMembersSummary -> Text
renderMembers members =
  case members of
    ExportNoMembersEMS -> ""
    ExportAllMembersEMS -> ".."
    ExportSelectedMembersEMS selected -> Tx.intercalate ", " $ V.toList selected