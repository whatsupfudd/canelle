module Cannelle.Elm.Parser.Types where

import Control.Monad.Identity (Identity(..))

import qualified Cannelle.TreeSitter.Error as E
import qualified Cannelle.TreeSitter.Scanner as Sc


type ParserP = Sc.ScannerT (E.ScanError E.TError) Identity
