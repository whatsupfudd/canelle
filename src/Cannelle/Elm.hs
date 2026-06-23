module Cannelle.Elm
  ( -- * AST
    Context(..)
  , Name(..)
  , QName(..)
  , ModuleDecl(..)
  , KindMod(..)
  , ImportDecl(..)
  , Exposing(..)
  , ExposeItem(..)
  , CtorExposure(..)
  , Decl(..)
  , AnnotationDecl(..)
  , ValueDecl(..)
  , AliasDecl(..)
  , UnionDecl(..)
  , Ctor(..)
  , TypeExpr(..)
  , TypeField(..)
  , PatternSummary(..)
  , PortDecl(..)
  , InfixDecl(..)
  , Assoc(..)
  , Comment(..)
  , Diagnostic(..)

    -- * API
  , ModuleApi(..)
  , TypeApi(..)
  , ValueApi(..)
  , TypeDecl(..)
  , TypeExposure(..)
  , fromContext

    -- * Parsing
  , parse
  , parseContent
  , parseApi
  , parseApiContent

    -- * Resolving
  , resolveApi
  , DeclIndex(..)
  , SymbolKey(..)

    -- * Printing
  , printContext
  , printApi
  , renderContext
  , renderApi
  , renderTE
  , slice
  ) where

import Cannelle.Elm.AST
import Cannelle.Elm.API
import Cannelle.Elm.Parse
import Cannelle.Elm.Print
import Cannelle.Elm.Resolve