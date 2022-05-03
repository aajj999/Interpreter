module Main where

import LexExp
import ParExp
import AbsExp
import Interpret
import ErrM

main = do
  interact program
  putStrLn ""

program p = 
  let Ok e = pProgram (myLexer p) 
  in show (interpret e)