module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import AbsAJ   ()
import LexAJ   ( Token, mkPosToken )
import ParAJ   ( pProgram, myLexer )
import PrintAJ ( Print, printTree )
import SkelAJ  ()

type Err        = Either String
type ParseFun a = [Token] -> Err a

putStrV :: String -> IO ()
putStrV = putStrLn

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV "Tokens:"
      mapM_ (putStrV . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  putStrV $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: ./interpreter "
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  (files)         Parse content of files."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs