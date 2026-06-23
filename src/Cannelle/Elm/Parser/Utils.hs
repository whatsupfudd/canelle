module Cannelle.Elm.Parser.Utils where

import Data.Text (Text, pack)

import TreeSitter.Node (TSPoint(..))

import Cannelle.TreeSitter.Types (SegmentPos (..), NodeEntry(..))
import Cannelle.Elm.Parser.Types (ParserP)


currentSpan :: ParserP SegmentPos
currentSpan =
  pure syntheticSpan

syntheticSpan :: SegmentPos
syntheticSpan =
  (syntheticPoint, syntheticPoint)

syntheticPoint :: TSPoint
syntheticPoint = TSPoint 0 0

spanNE :: NodeEntry -> SegmentPos
spanNE ne =
  (start ne, end ne)

tshow :: Show a => a -> Text
tshow =
  pack . show