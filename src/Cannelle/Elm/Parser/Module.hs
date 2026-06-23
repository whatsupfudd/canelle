module Cannelle.Elm.Parser.Module
  ( scanModule
  , scanQName
  , scanName
  , scanUpperName
  , scanLowerName
  ) where

import Control.Applicative ((<|>), asum)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types (SegmentPos, NodeEntry(..))
import TreeSitter.Node (TSPoint(..))

import Cannelle.Elm.AST
import qualified Cannelle.Elm.Parser.Expose as ExpP
import Cannelle.Elm.Parser.Types (ParserP)
import Cannelle.Elm.Parser.Utils (currentSpan, syntheticSpan)

scanModule :: ParserP ModuleDecl
scanModule = do
  ne <- Sc.singleP "module_declaration"
  kind <- scanKindMod
  name <- scanQName
  exposing <- ExpP.scanExpose
  pure $ ModuleDecl { kindMod = kind, nameMod = name, exposingMod = exposing, spanMod = (ne.start, ne.end) }


scanKindMod :: ParserP KindMod
scanKindMod = asum [
    scanPortKind
  , scanEffectKind
  , scanNormalKind
  ]


scanNormalKind :: ParserP KindMod
scanNormalKind = do
  _ <- Sc.single "module"
  pure NormalKM

scanPortKind :: ParserP KindMod
scanPortKind = do
  _ <- Sc.single "port"
  _ <- Sc.single "module"
  pure PortKM

scanEffectKind :: ParserP KindMod
scanEffectKind = do
  _ <- Sc.single "effect"
  _ <- Sc.single "module"
  pure EffectKM

scanQName :: ParserP QName
scanQName =
  asum
    [ scanUpperQid
    , QName . V.singleton <$> scanUpperName
    ]


scanUpperQid :: ParserP QName
scanUpperQid = do
  Sc.singleP "upper_case_qid"
  parts <- scanUpperName `Sc.sepBy1` Sc.single "dot"
  pure $ QName $ V.fromList parts


scanName :: ParserP Name
scanName =
  scanUpperName <|> scanLowerName

scanUpperName :: ParserP Name
scanUpperName =
  scanSymbolAsName "upper_case_identifier"

scanLowerName :: ParserP Name
scanLowerName =
  scanSymbolAsName "lower_case_identifier"

scanSymbolAsName :: String -> ParserP Name
scanSymbolAsName nodeName = do
  demand <- Sc.symbol nodeName
  pure $
    Name
      { textName = T.pack (show demand)
      , spanName = syntheticSpan
      , demandName = Just demand
      }
