module Interpreter where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsAJ
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe

type Err = Either String
type Result = Err String
type Env a = Map.Map AbsAJ.Ident (AbsAJ.AJr' a)

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

interpret :: Show a => AbsAJ.Program' a -> Result
interpret x = case x of
  AbsAJ.Program _ globals topdefs -> transTopDefs topdefs v where
    v = transGlobals globals Map.empty
    
transGlobals :: Show a => [AbsAJ.Global' a] -> Env a -> Env a
transGlobals [] v = v
transGlobals (x : xs) v = case x of
  AbsAJ.GlobalDef _ t id value -> transGlobals xs v2 where
    v2 = transGlobal x v

transGlobal :: Show a => AbsAJ.Global' a -> Env a -> Env a
transGlobal x v = case x of
  AbsAJ.GlobalDef a_ type_ ident expr -> v' where
    v' = Map.insert ident expr v

transTopDefs :: Show a => [AbsAJ.TopDef' a] -> Env a -> Result
transTopDefs [] _ = failure "aaa"
transTopDefs (x : xs)  v = case x of
  AbsAJ.FnDef _a _type _ident _args _block -> do
    res <- transTopDef x v
    transTopDefs xs v

transTopDef :: Show a => AbsAJ.TopDef' a -> Env a -> Result
transTopDef x v = case x of
  AbsAJ.FnDef _ type_ ident args block -> do
    transType type_
    transIdent ident
    transArgs args
    transBlock block

transIdent :: AbsAJ.Ident -> Env a -> (Maybe (AbsAJ.AJr' a))
transIdent x v = case x of
  AbsAJ.Ident string -> Map.lookup x v

transArgs :: Show a => [AbsAJ.Arg' a] -> Result
transArgs (x : xs) = case x of
  AbsAJ.Arg _ type_ ident -> failure ident

transArg :: Show a => AbsAJ.Arg' a -> Result
transArg x = case x of
  AbsAJ.Arg _ type_ ident -> failure x
  AbsAJ.InArg _ type_ ident -> failure x
  AbsAJ.OutArg _ type_ ident -> failure x

transBlock :: Show a => AbsAJ.Block' a -> Result
transBlock x = case x of
  AbsAJ.Block _ stmts -> failure x

transStmt :: Show a => AbsAJ.Stmt' a -> Result
transStmt x = case x of
  AbsAJ.Empty _ -> failure x
  AbsAJ.BStmt _ block -> failure x
  AbsAJ.Decl _ type_ items -> failure x
  AbsAJ.Ass _ ident expr -> failure x
  AbsAJ.Incr _ ident -> failure x
  AbsAJ.Decr _ ident -> failure x
  AbsAJ.Ret _ expr -> failure x
  AbsAJ.Cond _ expr stmt -> failure x
  AbsAJ.CondElse _ expr stmt1 stmt2 -> failure x
  AbsAJ.While _ expr stmt -> failure x
  AbsAJ.SAJ _ expr -> failure x
  AbsAJ.Print _ expr -> failure x

transItem :: Show a => AbsAJ.Item' a -> Result
transItem x = case x of
  AbsAJ.NoInit _ ident -> failure x
  AbsAJ.Init _ ident expr -> failure x

transType :: Show a => AbsAJ.Type' a -> Result
transType x = case x of
  AbsAJ.Int _ -> failure x
  AbsAJ.Str _ -> failure x
  AbsAJ.Bool _ -> failure x
  AbsAJ.Fun _ type_ types -> failure x

transAJr :: Show a => AbsAJ.AJr' a -> AbsAJr.Ex
transAJr x = case x of
  AbsAJ.EVar _ ident -> failure x
  AbsAJ.ELitInt _ integer -> failure x
  AbsAJ.ELitTrue _ -> failure x
  AbsAJ.ELitFalse _ -> failure x
  AbsAJ.EApp _ ident exprs -> failure x
  AbsAJ.EString _ string -> failure x
  AbsAJ.Neg _ expr -> failure x
  AbsAJ.Not _ expr -> failure x
  AbsAJ.EMul _ expr1 mulop expr2 -> failure x
  AbsAJ.EAdd _ 0 e -> e
  AbsAJ.EAdd _ e 0 -> e
  AbsAJ.EAdd _ (AbsAJ.Int a) (AbsAJ.Int b) -> AbsAJ.Int $ a + b
  AbsAJ.EAdd p expr1 addop expr2 -> transAJr $ AbsAJ.EAdd p (transAJr expr1) (transAJr expr2)
  AbsAJ.ERel _ expr1 relop expr2 -> transRelOp relop expr1 expr2
  AbsAJ.EAnd _ expr1 expr2 -> failure x
  AbsAJ.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => AbsAJ.AddOp' a -> Result
transAddOp x = case x of
  AbsAJ.Plus _ -> failure x
  AbsAJ.Minus _ -> failure x

transMulOp :: Show a => AbsAJ.MulOp' a -> AbsAJ.Int
transMulOp x a b = case x of
  AbsAJ.Times _ -> a * b
  AbsAJ.Div _ -> a / b
  AbsAJ.Mod _ -> a % b

transRelOp :: Show a => AbsAJ.RelOp' a -> AbsAJ.AJr' a -> AbsAJ.AJr' a -> Either ELitFalse ELitTrue
transRelOp x a b = case x of
  AbsAJ.LTH _ -> a < b
  AbsAJ.LE _ -> a <= b
  AbsAJ.GTH _ -> a > b
  AbsAJ.GE _ -> a >= b
  AbsAJ.EQU _ -> a == b
  AbsAJ.NE _ -> a != b
