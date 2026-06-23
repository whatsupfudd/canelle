module Cannelle.Elm.Parse
  ( parse
  , parseContent
  , parseApi
  , parseApiContent
  , parseTsAst
  ) where

import Control.Monad (when)

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Foreign.Ptr (Ptr)

import TreeSitter.Elm (tree_sitter_elm)
import TreeSitter.Node (Node)
import TreeSitter.Parser (ts_parser_new, ts_parser_set_language)

import Cannelle.Common.Error (CompError(..))
import Cannelle.Common.TsAST (analyzeChildren, tryParseFromContent)
import Cannelle.TreeSitter.Print (printNode)

import qualified Cannelle.Elm.API as Api
import Cannelle.Elm.API (ModuleApi)
import Cannelle.Elm.AST (Context)
import qualified Cannelle.Elm.Parser as Parser
import qualified Cannelle.Elm.Print as Print
import qualified Cannelle.Elm.Resolve as Rsv


parse :: Bool -> FilePath -> IO (Either CompError Context)
parse debugMode path = do
  when debugMode $ putStrLn $ "@[elm/parse] parsing: " <> path
  content <- Bs.readFile path
  parseContent debugMode path content Nothing


parseContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath -> IO (Either CompError Context)
parseContent debugMode filePath content _mbOutPath = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_elm
  tryParseFromContent debugMode parser (parseTsAst content) filePath content


parseApi :: Bool -> FilePath -> IO (Either CompError ModuleApi)
parseApi debugMode path = do
  content <- Bs.readFile path
  parseApiContent debugMode path content Nothing


parseApiContent :: Bool -> FilePath -> Bs.ByteString -> Maybe FilePath
      -> IO (Either CompError ModuleApi)
parseApiContent debugMode filePath content mbOutPath = do
  rez <- parseContent debugMode filePath content mbOutPath
  case rez of
    Left err -> pure $ Left err
    Right ctx -> pure $ Right $ Rsv.resolveApi filePath ctx


parseTsAst :: Bs.ByteString -> Bool -> Ptr Node -> Int -> IO (Either CompError Context)
parseTsAst content debugMode children count = do
  startA <- getCurrentTime
  nodeGraph <- analyzeChildren 0 children count
  endA <- getCurrentTime

  when debugMode $
    putStrLn $ "@[elm/parseTsAst] analyzeChildren time: " <> show (diffUTCTime endA startA)

  when debugMode $
    mapM_ (printNode 0) nodeGraph

  startB <- getCurrentTime
  let scanRez = Parser.scan nodeGraph
  endB <- getCurrentTime

  when debugMode $
    putStrLn $ "@[elm/parseTsAst] scan time: " <> show (diffUTCTime endB startB)

  case scanRez of
    Left err -> pure . Left $ CompError [ (0 :: Int32, "@[elm/parseTsAst] scan err: " <> show err) ]
    Right ctx -> do
      when debugMode $
        Print.printContext content debugMode "<filepath>" ctx
      pure $ Right ctx