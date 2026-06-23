module Cannelle.Elm.Parser.Expose
  ( scanExpose
  , scanExposeItem
  , scanCtorExposure
  ) where

import Control.Applicative (optional, (<|>))
import Control.Monad.Identity (Identity(..))

import Data.Foldable (asum)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types (SegmentPos)

import Cannelle.Elm.AST (CtorExposure(..), ExposeItem(..), Exposing(..), Name(..))
import Cannelle.Elm.Parser.Types (ParserP)
import Cannelle.Elm.Parser.Utils (syntheticSpan)


scanExpose :: ParserP Exposing
scanExpose = do
  Sc.singleP "exposing_list"
  Sc.single "exposing"
  Sc.single "("
  rez <- scanAll <|> scanExplicit
  Sc.single ")"
  pure rez

scanAll :: ParserP Exposing
scanAll = do
  Sc.single "double_dot"
  pure $ AllEX syntheticSpan

scanExplicit :: ParserP Exposing
scanExplicit = do
  items <- scanExposeItem `Sc.sepBy1` Sc.single ","
  pure $ ExplicitEX (V.fromList items) syntheticSpan

scanExposeItem :: ParserP ExposeItem
scanExposeItem =
  asum
    [ scanValueEI
    , scanTypeEI
    , scanOperatorEI
    ]

scanValueEI :: ParserP ExposeItem
scanValueEI = do
  Sc.singleP "exposed_value"
  name <- scanLowerName
  pure $ ValueEI name


scanTypeEI :: ParserP ExposeItem
scanTypeEI = do
  Sc.singleP "exposed_type"
  name <- scanUpperName
  ctorExp <- optional scanCtorExposure
  pure $ TypeEI name $ maybe HiddenCE id ctorExp


scanCtorExposure :: ParserP CtorExposure
scanCtorExposure = do
  Sc.singleP "exposed_union_constructors"
  optional $ Sc.single "("
  Sc.single "double_dot"
  optional $ Sc.single ")"
  pure ExposedCE


scanOperatorEI :: ParserP ExposeItem
scanOperatorEI = do
  Sc.singleP "exposed_operator"
  optional $ Sc.single "("
  name <- scanOperatorName
  optional $ Sc.single ")"
  pure $ OperatorEI name


scanLowerName :: ParserP Name
scanLowerName =
  scanSymbolName "lower_case_identifier"

scanUpperName :: ParserP Name
scanUpperName =
  scanSymbolName "upper_case_identifier"

scanOperatorName :: ParserP Name
scanOperatorName =
  asum
    [ scanSymbolName "operator_identifier"
    , scanSymbolName "operator"
    , scanSymbolName "operator_symbol"
    ]

scanSymbolName :: String -> ParserP Name
scanSymbolName nodeName = do
  demand <- Sc.symbol nodeName
  pure $
    Name
      { textName = placeholderText demand
      , spanName = syntheticSpan
      , demandName = Just demand
      }

placeholderText :: Int -> T.Text
placeholderText demand =
  T.cons '#' $ T.pack $ show demand
