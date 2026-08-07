module Cannelle.Haskell.Parser.Names (
    identifierS, shortIdentS, nameIdentS, varIdentS
    , moduleNameS, moduleIdS, qualifiedNameS, qualifiedModuleNameS
  ) where

import Control.Applicative (asum)

import qualified Cannelle.TreeSitter.Scanner as S
import Cannelle.Parser.Debug (debugOpt)

import Cannelle.Haskell.AST
import Cannelle.Haskell.Parser.Types


identifierS :: ScannerP Identifier
identifierS = asum [ shortIdentS, qualifiedNameS ]


shortIdentS :: ScannerP Identifier
shortIdentS = asum [ nameIdentS, varIdentS ]


nameIdentS :: ScannerP Identifier
nameIdentS = NameIdent <$> S.symbol "name"


varIdentS :: ScannerP Identifier
varIdentS = VarIdent <$> S.symbol "variable"


moduleNameS :: ScannerP [Identifier]
moduleNameS = do
  S.singleP "module"
  moduleIdS `S.sepBy` S.single "."


moduleIdS :: ScannerP Identifier
moduleIdS = NameIdent <$> S.symbol "module_id"


qualifiedNameS :: ScannerP Identifier
qualifiedNameS = debugOpt "qs-qualified" $ do
  S.singleP "qualified"
  moduleName <- qualifiedModuleNameS
  QualIdent moduleName <$> shortIdentS


qualifiedModuleNameS :: ScannerP [Identifier]
qualifiedModuleNameS = moduleNameS <* S.single "."