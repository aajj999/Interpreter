module Interpret where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsExp
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe

type Err = Either String
type Result = Err String
type Env a = Map.Map AbsExp.Ident (AbsExp.Expr' a)

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

interpret :: Show a => AbsExp.Program' a -> Result
interpret x = case x of
  AbsExp.Program _ globals topdefs -> transTopDefs topdefs v where
    v = transGlobals globals Map.empty
    
transGlobals :: Show a => [AbsExp.Global' a] -> Env a -> Env a
transGlobals [] v = v
transGlobals (x : xs) v = case x of
  AbsExp.GlobalDef _ t id value -> transGlobals xs v2 where
    v2 = transGlobal x v

transGlobal :: Show a => AbsExp.Global' a -> Env a -> Env a
transGlobal x v = case x of
  AbsExp.GlobalDef a_ type_ ident expr -> v' where
    v' = Map.insert ident expr v

transTopDefs :: Show a => [AbsExp.TopDef' a] -> Env a -> Result
transTopDefs [] _ = failure "aaa"
transTopDefs (x : xs)  v = case x of
  AbsExp.FnDef _a _type _ident _args _block -> do
    res <- transTopDef x v
    transTopDefs xs v

transTopDef :: Show a => AbsExp.TopDef' a -> Env a -> Result
transTopDef x v = case x of
  AbsExp.FnDef _ type_ ident args block -> do
    transType type_
    transIdent ident
    transArgs args
    transBlock block

transIdent :: AbsExp.Ident -> Env a -> (Maybe (AbsExp.Expr' a))
transIdent x v = case x of
  AbsExp.Ident string -> Map.lookup x v

transArgs :: Show a => [AbsExp.Arg' a] -> Result
transArgs (x : xs) = case x of
  AbsExp.Arg _ type_ ident -> failure ident

transArg :: Show a => AbsExp.Arg' a -> Result
transArg x = case x of
  AbsExp.Arg _ type_ ident -> failure x
  AbsExp.InArg _ type_ ident -> failure x
  AbsExp.OutArg _ type_ ident -> failure x

transBlock :: Show a => AbsExp.Block' a -> Result
transBlock x = case x of
  AbsExp.Block _ stmts -> failure x

transStmt :: Show a => AbsExp.Stmt' a -> Result
transStmt x = case x of
  AbsExp.Empty _ -> failure x
  AbsExp.BStmt _ block -> failure x
  AbsExp.Decl _ type_ items -> failure x
  AbsExp.Ass _ ident expr -> failure x
  AbsExp.Incr _ ident -> failure x
  AbsExp.Decr _ ident -> failure x
  AbsExp.Ret _ expr -> failure x
  AbsExp.Cond _ expr stmt -> failure x
  AbsExp.CondElse _ expr stmt1 stmt2 -> failure x
  AbsExp.While _ expr stmt -> failure x
  AbsExp.SExp _ expr -> failure x
  AbsExp.Print _ expr -> failure x

transItem :: Show a => AbsExp.Item' a -> Result
transItem x = case x of
  AbsExp.NoInit _ ident -> failure x
  AbsExp.Init _ ident expr -> failure x

transType :: Show a => AbsExp.Type' a -> Result
transType x = case x of
  AbsExp.Int _ -> failure x
  AbsExp.Str _ -> failure x
  AbsExp.Bool _ -> failure x
  AbsExp.Fun _ type_ types -> failure x

transExpr :: Show a => AbsExp.Expr' a -> Result
transExpr x = case x of
  AbsExp.EVar _ ident -> failure x
  AbsExp.ELitInt _ integer -> failure x
  AbsExp.ELitTrue _ -> failure x
  AbsExp.ELitFalse _ -> failure x
  AbsExp.EApp _ ident exprs -> failure x
  AbsExp.EString _ string -> failure x
  AbsExp.Neg _ expr -> failure x
  AbsExp.Not _ expr -> failure x
  AbsExp.EMul _ expr1 mulop expr2 -> failure x
  AbsExp.EAdd _ expr1 addop expr2 -> failure x
  AbsExp.ERel _ expr1 relop expr2 -> failure x
  AbsExp.EAnd _ expr1 expr2 -> failure x
  AbsExp.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => AbsExp.AddOp' a -> Result
transAddOp x = case x of
  AbsExp.Plus _ -> failure x
  AbsExp.Minus _ -> failure x

transMulOp :: Show a => AbsExp.MulOp' a -> Result
transMulOp x = case x of
  AbsExp.Times _ -> failure x
  AbsExp.Div _ -> failure x
  AbsExp.Mod _ -> failure x

transRelOp :: Show a => AbsExp.RelOp' a -> Result
transRelOp x = case x of
  AbsExp.LTH _ -> failure x
  AbsExp.LE _ -> failure x
  AbsExp.GTH _ -> failure x
  AbsExp.GE _ -> failure x
  AbsExp.EQU _ -> failure x
  AbsExp.NE _ -> failure x
