module Cannelle.Haskell.Parser where

import Control.Applicative (many)

import qualified Data.Vector as V

import qualified Cannelle.TreeSitter.Scanner as Sc
import qualified Cannelle.TreeSitter.Error as E
import Cannelle.TreeSitter.Types (NodeEntry (..))

import Cannelle.Haskell.Parser.Recovery (appendSyntaxErrors, collectSyntaxErrorDecls)

import Cannelle.Haskell.Parser.Types (ScannerP)
import Cannelle.Haskell.AST (HaskellContext (..))
import Cannelle.Haskell.Parser.Statements (haskellS)


haskellScanner :: [NodeEntry] -> Either E.TError HaskellContext
haskellScanner nodes =
  let
    mainScanner = haskellS <* Sc.pEof
    syntaxErrors = collectSyntaxErrorDecls nodes
  in
  case Sc.doScan mainScanner nodes of
    Left err -> Left . E.TError $ E.showScanErrorBundle err
    Right (context, positions) ->
      let
        declarations = appendSyntaxErrors context.declarations syntaxErrors
      in
      Right context { declarations = declarations, contentDemands = positions }
