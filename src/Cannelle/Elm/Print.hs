module Cannelle.Elm.Print
  ( printContext
  , printApi
  , renderContext
  , renderApi
  , renderTE
  , slice
  ) where

import qualified Data.ByteString as Bs
import qualified Data.Map as Mp
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Vector (Vector)

import Cannelle.TreeSitter.Types ( SegmentPos )
import Cannelle.TreeSitter.Print (fetchContentBs)

import Cannelle.Elm.AST
import Cannelle.Elm.API
import Cannelle.Elm.Parser.Utils (tshow)
import TreeSitter.Node (TSPoint(..))
import Data.Maybe (isJust, fromJust)


printContext :: Bs.ByteString -> Bool -> FilePath -> Context -> IO ()
printContext content dbgMode pathName ctx =
  TIO.putStrLn $ renderContext content dbgMode pathName ctx


renderContext :: Bs.ByteString -> Bool -> FilePath -> Context -> Text
renderContext content dbgMode pathName ctx =
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContentBs cLines) $ V.zip ctx.demandsCtx (V.fromList [0..])
  in
  if dbgMode then
    "@[elm/context]\nmodule: " <> renderMaybeModule demandLines ctx.moduleCtx
    <> "\n--- imports:\n" <> T.intercalate "\n" (fmap (renderImport demandLines) $ V.toList ctx.importsCtx)
    <> "\n--- decls:\n" <> T.intercalate "\n" (fmap (renderDecl demandLines True) $ V.toList ctx.declsCtx)
    <> "\n--- demands:\n" <> renderDemandLines demandLines ctx.demandsCtx
    <> "\n--- # comments: " <> tshow (V.length ctx.commentsCtx)
    <> "\n--- # diagnostics: " <> tshow (V.length ctx.diagsCtx)
  else
    let
      decls = case ctx.moduleCtx of
        Just modDecl -> case modDecl.exposingMod of
          AllEX _ -> fmap (renderDecl demandLines False) $ V.toList ctx.declsCtx
          ExplicitEX items _ ->
            let
              expoItems = map (\(aName, item) -> (fromJust aName.demandName, item)) $
                filter (\(aName, item) -> isJust $ aName.demandName) [ (getExpItemName item, item) | item <- V.toList items ]
              itemMap = Mp.fromList $ map (\(jMbName, item) -> (fromJust jMbName, item) ) $ filter (\(mbLine, _) -> isJust mbLine) [ (demandLines V.!? dName, item) | (dName, item) <- expoItems ]
              exposedDecls = [ decl | decl <- V.toList ctx.declsCtx, isExposedDecl demandLines itemMap decl ]
            in
            fmap (renderDecl demandLines False) $ exposedDecls
        Nothing -> fmap (renderDecl demandLines False) $ V.toList ctx.declsCtx
    in
    "The file '" <> T.pack pathName <> "' top-level description is:<elm_extract>\n"
    <> case ctx.moduleCtx of
        Just modDecl -> "module " <> renderModule demandLines modDecl <> "\n\n"
        Nothing -> "<none>"
    <> T.intercalate "\n" decls
    <> "\n</elm_extract>\n"


isExposedDecl :: Vector Bs.ByteString -> Mp.Map Bs.ByteString ExposeItem -> Decl -> Bool
isExposedDecl demandLines itemMap decl =
  let
    mbDName = case decl of
      AnnotationD ann -> ann.nameAnn.demandName
      ValueD value -> value.nameVal.demandName
      AliasD alias -> alias.nameAlias.demandName
      UnionD union -> union.nameUnion.demandName
      PortD port -> port.namePort.demandName
      InfixD vInfix -> vInfix.nameInfix.demandName
      UnhandledD label _ -> Nothing
  in
  case mbDName of
    Just dName -> case demandLines V.!? dName of
      Just line -> Mp.member line itemMap
      Nothing -> False
    Nothing -> False


renderMaybeModule :: Vector Bs.ByteString -> Maybe ModuleDecl -> Text
renderMaybeModule _ Nothing = "<none>"
renderMaybeModule lines (Just modDecl) = do
  renderModule lines modDecl

renderModule :: Vector Bs.ByteString -> ModuleDecl -> Text
renderModule lines modDecl =
  textQName lines modDecl.nameMod <> " exposing " <> renderExposing lines modDecl.exposingMod


printApi :: Bs.ByteString -> ModuleApi -> IO ()
printApi content api =
  TIO.putStrLn $ renderApi content api

renderApi :: Bs.ByteString -> ModuleApi -> Text
renderApi content api =
  let
    cLines = V.fromList $ Bs.split 10 content
    demandLines = V.map (fetchContentBs cLines) $ V.zip api.demandsApi (V.fromList [0..])
  in
  let
    decls = fmap (renderValueApi demandLines) $ V.toList api.valuesApi
  in
  "The file '" <> T.pack api.pathApi <> "' top-level description is:<haskel_extract>\n"
  <> case api.declModApi of
      Just modDecl -> "module " <> renderModule demandLines modDecl
      Nothing -> "<none>"
  <> "\n--- types: " <> T.intercalate "\n" (fmap (renderTypeApi demandLines) $ V.toList api.typesApi)
  <> "\n--- values: " <> T.intercalate "\n" decls
  -- tshow (V.length $ api.valuesApi)
  <> "\n--- diagnostics: " <> tshow (V.length $ api.diagsApi)
  --  T.intercalate "\n" (V.toList $ V.map (T.pack . show) api.diagsApi)


renderExposing :: Vector Bs.ByteString -> Exposing -> Text
renderExposing lines exposing =
  case exposing of
    AllEX sp -> "<all>"
    ExplicitEX items sp ->
      "(" <> T.intercalate ", " (fmap (renderExposeItem lines) $ V.toList items) <> ")"


renderExposeItem :: Vector Bs.ByteString -> ExposeItem -> Text
renderExposeItem lines item =
  case item of
    ValueEI name -> mbTextName lines name.demandName
    TypeEI name exposure -> mbTextName lines name.demandName <> case exposure of
      HiddenCE -> ""
      ExposedCE ->  "(..)"
    OperatorEI name -> mbTextName lines name.demandName


renderImport :: Vector Bs.ByteString -> ImportDecl -> Text
renderImport lines importDecl =
  textQName lines importDecl.moduleImp
  <> case importDecl.aliasImp of
       Just alias -> " as " <> textQName lines alias
       Nothing -> ""
 <> case importDecl.exposingImp of
       Just exposing -> " exposing " <> renderExposing lines exposing
       Nothing -> ""


renderDecl :: Vector Bs.ByteString -> Bool -> Decl -> Text
renderDecl lines fullMode decl =
  case decl of
    AnnotationD ann -> renderAnnotation lines ann
    ValueD value -> renderValue lines fullMode value
    AliasD alias -> renderAlias lines alias <> "\n"
    UnionD union -> renderUnion lines union <> "\n"
    PortD port -> renderPort lines port
    InfixD vInfix -> renderInfix lines vInfix
    UnhandledD label _ -> "<unhandled decl: " <> label <> ">"


renderAnnotation :: Vector Bs.ByteString -> AnnotationDecl -> Text
renderAnnotation lines annotation =
  mbTextName lines annotation.nameAnn.demandName <> " : " <> renderTE lines annotation.exprAnn


renderValue :: Vector Bs.ByteString -> Bool -> ValueDecl -> Text
renderValue lines fullMode value =
  if fullMode then
    mbTextName lines value.nameVal.demandName
    <> if V.null value.argsVal then "" else " "
    <> T.intercalate " " (fmap (renderPattern lines) $ V.toList value.argsVal)
  else
    ""


renderPattern :: Vector Bs.ByteString -> PatternSummary -> Text
renderPattern lines pattern =
  case pattern of
    NamePS name -> mbTextName lines name.demandName
    IgnoredPS sp -> "<ignored>"
    TuplePS patterns sp -> "<tuple>"
    RecordPS fields sp -> "<record>"
    UnhandledPS label sp -> "<unhandled pattern: " <> label <> ">"


renderAlias :: Vector Bs.ByteString -> AliasDecl -> Text
renderAlias lines alias =
  "type alias " <> mbTextName lines alias.nameAlias.demandName
  <> T.intercalate " " (fmap (renderName lines) $ V.toList alias.paramsAlias)
  <> " = " <> renderTE lines alias.exprAlias


renderName :: Vector Bs.ByteString -> Name -> Text
renderName lines name =
  mbTextName lines name.demandName


renderUnion :: Vector Bs.ByteString -> UnionDecl -> Text
renderUnion lines union =
  "type " <> mbTextName lines union.nameUnion.demandName
  <> T.intercalate " " (fmap (renderName lines) $ V.toList union.paramsUnion)
  <> " =\n    " <> T.intercalate "\n  | " (fmap (renderCtor lines) $ V.toList union.ctorsUnion)


renderCtor :: Vector Bs.ByteString -> Ctor -> Text
renderCtor lines ctor =
  mbTextName lines ctor.nameCtor.demandName
  <> if V.null ctor.argsCtor then "" else " "
  <> T.intercalate " " (fmap (renderTE lines) $ V.toList ctor.argsCtor)


renderPort :: Vector Bs.ByteString -> PortDecl -> Text
renderPort lines port =
  "port " <> mbTextName lines port.namePort.demandName
  <> " : " <> renderTE lines port.exprPort


renderInfix :: Vector Bs.ByteString -> InfixDecl -> Text
renderInfix lines vInfix =
  mbTextName lines vInfix.nameInfix.demandName
  <> " " <> renderAssoc vInfix.assocInfix
  <> " " <> T.pack (show vInfix.precInfix)

renderAssoc :: Assoc -> Text
renderAssoc assoc =
  case assoc of
    LeftA -> "left"
    RightA -> "right"
    NonA -> "non"

renderTE :: Vector Bs.ByteString -> TypeExpr -> Text
renderTE lines expr =
  case expr of
    VarTE name -> mbTextName lines name.demandName
    RefTE qn args ->
      let
        base = textQName lines qn
        renderedArgs = fmap (renderTE lines) args
      in
      case V.toList renderedArgs of
        [] -> base
        rest -> base <> " " <> T.unwords rest
    RecordTE mbName fields ->
      "{\n    " <> T.intercalate "\n  , " (fmap (renderField lines) $ V.toList fields) <> "\n  }"
    TupleTE items -> "( " <> T.intercalate ", " (V.toList $ fmap (renderTE lines) items) <> " )"
    FunTE left right -> renderTE lines left <> " -> " <> renderTE lines right
    ParenTE inner -> "(" <> renderTE lines inner <> ")"
    UnitTE _ -> "()"
    UnhandledTE label sp -> "<unhandled type expr: " <> label <> "; " <> T.pack (show sp) <> ">"


renderField :: Vector Bs.ByteString -> TypeField -> Text
renderField lines field =
  mbTextName lines field.nameTF.demandName <> " : " <> renderTE lines field.exprTF

slice :: Bs.ByteString -> SegmentPos -> Text
slice content sp =
  "sp: " <> T.pack (show sp)


renderDemands :: Bs.ByteString -> Vector SegmentPos -> Text
renderDemands content demands =
  let
    lines = V.fromList $ Bs.split 10 content
  in
  T.intercalate "\n  " (fmap (T.pack . show) $ V.toList demands)


renderDemandLines :: Vector Bs.ByteString -> Vector SegmentPos -> Text
renderDemandLines lines demands =
  let
    allItems = V.zip3 lines demands (V.fromList [0..])
  in
  T.intercalate "\n" (fmap showDemandItem $ V.toList allItems)
  where
  showDemandItem :: (Bs.ByteString, SegmentPos, Int) -> Text
  showDemandItem (line, demand@(start, end), index) =
    T.pack (show index) <> ": " <> (decodeUtf8 line)
    <> T.pack (
        " | "<> show start.pointRow <> ":" <> show start.pointColumn
        <> "-"
        <> show end.pointRow <> ":" <> show end.pointColumn
      )

---- API rendering

renderTypeApi :: Vector Bs.ByteString -> TypeApi -> Text
renderTypeApi lines typeApi =
  case typeApi.declTA of
    AliasTD alias -> renderAlias lines alias
    UnionTD union -> renderUnion lines union


renderValueApi :: Vector Bs.ByteString -> ValueApi -> Text
renderValueApi lines value =
  mbTextName lines value.nameVA.demandName <> " : "
  <> case value.annVA of
       Just ann -> " : " <> renderAnnotation lines ann
       Nothing -> ""
  <> case value.declVA of
       Just decl -> " = " <> renderValue lines False decl
       Nothing -> ""
  <> case value.diagsVA of
       diags -> " : " <> T.intercalate "\n" (fmap (renderDiagnostic lines) $ V.toList diags)


renderDiagnostic :: Vector Bs.ByteString -> Diagnostic -> Text
renderDiagnostic lines diagnostic =
  case diagnostic of
    ParseDiag text sp -> "<parse diag: " <> T.pack (show sp) <> ">"
    MissingModuleDiag file -> "<missing module: " <> T.pack file <> ">"
    ExposedNameDiag name sp -> "<exposed name: " <> name <> ">"
    DuplicateNameDiag name1 sp1 sp2 -> "<duplicate name: " <> name1 <> " and " <> (T.pack . show) sp2 <> ">"
    DuplicateModuleDiag sp -> "<duplicate module: " <> T.pack (show sp) <> ">"
    MissingSignatureDiag name sp -> "<missing signature: " <> name <> ">"
