{-# LANGUAGE BlockArguments #-}
module Interpreter where

import Prelude
import AbsAJ
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe
import Distribution.Simple.Setup (trueArg)
import Control.Monad.Except
import Control.Monad.State

type Location = Int
data Value = VInt Integer | VBool Bool | VString String deriving Show
type Variables = Map.Map Ident Location
type Locations = Map.Map Location Value

data Function = VFunc Type [Arg] Block
type Functions = Map.Map Ident Function

type Env = (Variables, Locations, Functions)
type AM a = ReaderT Env (ExceptT String (StateT Locations IO)) a

err :: IO()
err = putStrLn "Error"

umt = "Unmatching types"

env :: Variables -> Locations -> Functions -> Env
env var loc fun = (var, loc, fun)

newLoc :: Locations -> Location
newLoc l = if Map.null l then 0 else (do
           let p = foldr1 (\x y ->if x >= y then x else y) $ Map.keys l
           p + 1)

interpret :: Program -> IO()
interpret (Program _ globals topdefs) = do
  let e = execState (transGlobals globals) (env Map.empty Map.empty Map.empty)
  evalState (transTopDefs topdefs) e

transGlobals :: [Global] -> State Env ()
transGlobals [] = return()
transGlobals (x : xs) = do
  transGlobal x
  transGlobals xs

transGlobal :: Global -> State Env ()
transGlobal x = case x of
  GlobalDef _ t id expr -> do
    e <- get
    let val = evalState (transExpr expr) e
    case val of
      Left _ -> return()
      Right val -> do
        (var, loc, fun) <- get
        let l = newLoc loc
        let var' = Map.insert id l var
        let loc' = Map.insert l val loc
        put (var', loc', fun)

transTopDefs :: [TopDef] -> State Env (IO ())
transTopDefs [] = return $ putStrLn ""
transTopDefs (x : xs) = do
  case x of
    FnDef _ t (Ident "main") args block -> do
      transTopDef x
    FnDef _ t id args block -> do
      (var, loc, fun) <- get
      let fun' = Map.insert id (VFunc t args block) fun
      put (var, loc, fun')
      transTopDefs xs

transTopDef :: TopDef -> State Env (IO())
transTopDef (FnDef p _ _  _ b) = do
  gets (evalState (transStmt $ BStmt p  b))

transBlock :: [Stmt] -> State Env(IO ())
transBlock [] = return $ putStr ""
transBlock (s : ss) = do
  transStmt s
  transBlock ss

transStmt :: Stmt -> State Env (IO())
transStmt x = case x of
  Empty _ -> return $ putStr ""
  BStmt _ block -> case block of
    Block p [] -> return $ putStr ""
    Block p ss -> do
      (var, loc, fun) <- get
      transBlock ss
      put (var, loc, fun)
      return $ putStr ""
  Decl p t items -> case items of
    [] -> return $ putStr ""
    (i : is) -> do
      transItem t i
      transStmt $ Decl p t is
  Ass _ ident expr -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ putStr ""
      Just lo -> do
        let loc' = (case evalState (transExpr expr) (var, loc, fun) of
              Right x -> Map.insert lo x loc
              _ -> loc)
        put (var, loc', fun)
        return $ putStr ""
  Incr _ ident -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ putStr ""
      Just lo -> do
        let val = Map.lookup lo loc
        case val of
          Nothing -> return $ putStr ""
          Just valv -> do
            let loc' = Map.insert lo (valv + 1) loc
            put (var, loc', fun)
            return $ putStr ""
  Decr _ ident -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ putStr ""
      Just lo -> do
        let val = Map.lookup lo loc
        case val of
          Nothing -> return $ putStr ""
          Just valv -> do
            let loc' = Map.insert lo (valv + 1) loc
            put (var, loc', fun)
            return $ putStr ""
  Ret _ expr -> err
  Cond _ expr stmt -> err
  CondElse _ expr stmt1 stmt2 -> err
  While _ expr stmt -> err
  SExp _ expr -> err
  Print _ expr -> return $ print expr

transItem :: Type -> Item -> State Env ()
transItem t x = case x of
  NoInit _ ident -> do
    (var, loc, fun) <- get
    let l = newLoc loc
    let var' = Map.insert ident l var
    let loc' = (case t of
          Str _ -> Map.insert l (VString "") loc
          Int _ -> Map.insert l (VInt 0)  loc
          Bool _ -> Map.insert l (VBool False) loc
          _ -> loc)
    put (var', loc', fun)
  Init _ ident expr -> do
    (var, loc, fun) <- get
    let l = newLoc loc
    let var' = Map.insert ident l var
    let loc' = (case evalState (transExpr expr) (var, loc, fun) of
          Right val -> Map.insert l val loc
          _ -> loc)
    put (var', loc', fun)

transExpr :: Expr -> State Env (Either [Char] Value)
transExpr x = do
  e <- get
  case x of
    AbsAJ.ELitInt _ integer -> return $ Right $ VInt integer
    AbsAJ.ELitTrue _ -> return $ Right $ VBool True
    AbsAJ.ELitFalse _ -> return $ Right $ VBool False
    AbsAJ.EString _ string -> return $ Right $ VString string
    Neg _ expr -> case evalState (transExpr expr) e of
      Right (VInt int) -> return $ Right $ VInt $ -int
      _ -> return $ throwError "Wrong type for negative opearation"
    Not _ expr -> case evalState (transExpr expr) e of
      Right (VBool True) -> return $ Right $ VBool False
      Right (VBool False) -> return $ Right $ VBool True
      _ -> return $ throwError "Wrong type for not opearation"
    EMul _ expr1 mulop expr2 -> return $ evalState (transMulOp mulop expr1 expr2) e
    EAdd _ a op b -> return $ evalState (transAddOp op a b) e
    ERel _ expr1 relop expr2 -> return $ Right $ evalState (transRelOp relop expr1 expr2) e
    EAnd _ expr1 expr2 -> case evalState (transExpr expr1) e of
      Right (VBool True) -> case evalState (transExpr expr2) e of
        Right (VBool True) -> return $ Right $ VBool True
        _ -> return $ Right $ VBool False
      _ -> return $ Right $ VBool False
    EOr _ expr1 expr2 -> case evalState (transExpr expr1) e of
      Right (VBool True) -> return $ Right $ VBool True
      _ -> case evalState (transExpr expr2) e of
        Right (VBool True) -> return $ Right $ VBool True
        _ -> return $ Right $ VBool True
    EVar _ ident -> do
      (var, loc, _) <- get
      let l = Map.lookup ident var
      case l of
        Nothing -> return $ throwError "Not existing variable used"
        Just lo -> do
          let value = Map.lookup lo loc
          case value of
            Nothing -> return $ throwError "Not existing variable used"
            Just xv -> return $ Right xv
    EApp _ ident exprs -> return $ Right $ VString "aaa"


{-transIdentFunc :: Ident -> Env -> Env
transIdentFunc i v = v

transIdent :: Ident -> Variables -> Maybe Value
transIdent x v = case x of
  Ident string -> Map.lookup x v

transArgs :: [Arg] -> Variables -> Variables
transArgs [] v = v
transArgs (x : xs) v = transArgs xs v

transArg :: Show a => Arg' a -> IO()
transArg x = case x of
  Arg _ type_ ident -> err
  InArg _ type_ ident -> err
  OutArg _ type_ ident -> err

transBlock :: Block -> Env -> IO()
transBlock x v = case x of
  Block _ stmts -> do
    save env
    transStmt stmts
    put odl env-}

transAddOp :: AddOp -> Expr -> Expr -> State Env (Either [Char] Value)
transAddOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt av) -> case evalState (transExpr b) e of
      Right (VInt bv) -> case x of
        Plus _ -> return $ Right $ VInt $ av + bv
        Minus _ -> return $ Right  $VInt $ av - bv
      _ -> return $ throwError umt
    Right (VString as) -> case evalState (transExpr b) e of
      Right (VString bs) -> case x of
        Plus _ -> return $ Right $ VString $ as ++ bs
        _ -> return $ throwError "Not allowed opeartion on strings: -"
      _ -> return $ throwError umt
    _ -> return $ throwError $ "Using bool in opeartion" ++ show x

transMulOp :: MulOp -> Expr -> Expr -> State Env (Either [Char] Value)
transMulOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt av) -> case evalState (transExpr b) e of
      Right (VInt 0) -> case x of
        Times _ -> return $ Right $ VInt av
        Div p -> return $ throwError $ "Not allowed opearation div with integer 0" ++ show p
        Mod p -> return $ throwError $ "Not allowed opearation mod with integer 0" ++ show p
      Right (VInt bv) -> case x of
        Times _ -> return $ Right $ VInt $ av * bv
        Div _ -> return $ Right $ VInt $ div av bv
        Mod _ -> return $ Right $ VInt $ mod av bv
      _ -> return $ throwError umt
    _ -> return $ throwError "Wrong variable type for operation"

transRelOp :: RelOp -> Expr -> Expr -> State Env (Value)
transRelOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt va) -> case evalState (transExpr b) e of
      Right (VInt vb) -> case x of
        LTH _ -> return $ VBool $ va < vb
        LE _ -> return $ VBool $  va <= vb
        GTH _ -> return $ VBool $  va > vb
        GE _ -> return $ VBool $  va >=  vb
        EQU _ -> return $ VBool $  va ==  vb
        NE _ -> return $ VBool $  va /=  vb
      _ -> return $ VBool False
    Right (VString sa) -> case evalState (transExpr b) e of
      Right (VString sb) -> case x of
        LTH _ -> return $ VBool $  sa <  sb
        LE _ -> return $ VBool $  sa <= sb
        GTH _ -> return $ VBool $  sa > sb
        GE _ -> return $ VBool $  sa >=  sb
        EQU _ -> return $ VBool $  sa ==  sb
        NE _ -> return $ VBool $  sa /=  sb
      _ -> return $ VBool False
    _ -> return $ VBool False