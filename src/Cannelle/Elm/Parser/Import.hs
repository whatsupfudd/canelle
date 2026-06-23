module Cannelle.Elm.Parser.Import
  ( scanImport
  ) where

import Control.Applicative (optional)

import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc
import Cannelle.TreeSitter.Types (SegmentPos)

import Cannelle.Elm.AST
import qualified Cannelle.Elm.Parser.Expose as ExpP
import qualified Cannelle.Elm.Parser.Module as ModP

import Cannelle.Elm.Parser.Types (ParserP)



scanImport :: ParserP ImportDecl
scanImport = do
  Sc.singleP "import_clause"
  Sc.single "import"
  moduleName <- ModP.scanQName
  aliasName <- optional scanAlias
  exposingList <- optional ExpP.scanExpose
  pure $
    ImportDecl
      { moduleImp = moduleName
      , aliasImp = aliasName
      , exposingImp = exposingList
      , spanImp = spanImport moduleName aliasName exposingList
      }


scanAlias :: ParserP QName
scanAlias = do
  Sc.singleP "as_clause"
  Sc.single "as"
  ModP.scanQName


spanImport :: QName -> Maybe QName -> Maybe Exposing -> SegmentPos
spanImport moduleName aliasName exposingList =
  let startSp = spanQName moduleName
      endSp =
        case exposingList of
          Just exposingDecl ->
            spanExposing exposingDecl

          Nothing ->
            maybe startSp spanQName aliasName
   in mergeSpans startSp endSp

spanQName :: QName -> SegmentPos
spanQName qn =
  let parts = partsQName qn
   in if V.null parts
        then error "@[elm/parser/import] empty QName"
        else (fst $ spanName $ V.head parts, snd $ spanName $ V.last parts)

spanExposing :: Exposing -> SegmentPos
spanExposing exposingDecl =
  case exposingDecl of
    AllEX sp ->
      sp

    ExplicitEX _ sp ->
      sp

mergeSpans :: SegmentPos -> SegmentPos -> SegmentPos
mergeSpans (startSp, _) (_, endSp) =
  (startSp, endSp)