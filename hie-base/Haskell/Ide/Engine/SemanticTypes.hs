{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes
  ( WorkspaceEdit(..)
  , AST(..)
  , Diagnostic(..)
  , Position(..)
  , Range(..)
  , DiagnosticSeverity(..)
  , TextDocumentIdentifier(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  ) where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics
import           Language.Haskell.LSP.TH.DataTypesJSON (Diagnostic(..), Position(..), Range(..), DiagnosticSeverity(..), TextDocumentIdentifier(..), WorkspaceEdit(..), PublishDiagnosticsParams(..), List(..))

-- ---------------------------------------------------------------------

-- | GHC AST
data AST = AST {
    astModule      :: !T.Text
  , astParsed      :: !Value
  , astRenamed     :: !Value
  , astTypechecked :: !Value
  , astExports     :: !Value
  } deriving (Eq,Show,Generic)

instance ToJSON AST where
  toJSON (AST m p r t e) = object ["module" .= m, "parsed" .= p
    , "renamed" .= r, "typechecked" .= t, "exports" .= e ]
instance FromJSON AST where
  parseJSON = withObject "Ast" $ \v -> AST
    <$> v .: "module"
    <*> v .: "parsed"
    <*> v .: "renamed"
    <*> v .: "typechecked"
    <*> v .: "exports"

-- ---------------------------------------------------------------------
