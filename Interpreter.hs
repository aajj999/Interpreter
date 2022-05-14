module Interpreter where

import Prelude
import AbsAJ
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Maybe
import Distribution.Simple.Setup (trueArg)
import Control.Monad.Except
import Control.Monad.State
import Data.Time.Clock.POSIX (systemToPOSIXTime)

type Location = Int
data Value = VInt Integer | VBool Bool | VString String deriving Show
type Variables = Map.Map Ident Location
type Locations = Map.Map Location Value

data Function = VFunc Type Ident [Arg] Block
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
    let val = transExpr expr
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
  (var, loc, fun) <- get
  put(var, loc, fun)
  return $ print (var, loc)
{-transTopDefs [] _ = putStrLn ""
transTopDefs (x : xs)  v = case x of
  FnDef _a _type _ident _args _block -> do
    transTopDef x v
    transTopDefs xs v

transTopDef :: TopDef -> Env -> IO()
transTopDef x (v, f)= case x of
  FnDef _ type_ ident args block -> do
    v' <- transArgs args v
    transBlock block (v', f)

transIdentFunc :: Ident -> Env -> Env
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
    put odl env

transStmt :: Show a => Stmt' a -> IO()
transStmt x = case x of
  Empty _ -> err
  BStmt _ block -> err
  Decl _ type_ items -> err
  Ass _ ident expr -> err
  Incr _ ident -> err
  Decr _ ident -> err
  Ret _ expr -> err
  Cond _ expr stmt -> err
  CondElse _ expr stmt1 stmt2 -> err
  While _ expr stmt -> err
  SExp _ expr -> err
  Print _ expr -> putStrLn expr

transItem :: Type -> Item -> IO()
transItem t x = do
  (var, loc, fun) <- ask
  case x of
    NoInit _ ident -> do
      l <- newLoc
      var' <- Map.insert ident l var
      {-case t of
        Str -> loc' <- Map.insert l "" loc
        Int -> loc' <- Map.insert l 0  loc
        Bool -> loc' <- Map.insert l False loc-}
      put (var', loc', fun)
    Init _ ident expr -> do
      var' <- Map.insert ident l var
      loc' <- Map.insert l transExpr expr loc
      put(var', loc', fun)-}

transExpr :: Expr -> Either [Char] Value
transExpr x = case x of
  AbsAJ.ELitInt _ integer -> Right $ VInt integer
  AbsAJ.ELitTrue _ -> Right $ VBool True
  AbsAJ.ELitFalse _ -> Right $ VBool False
  AbsAJ.EString _ string -> Right $ VString string
  Neg _ expr -> case transExpr expr of
    Right (VInt int) -> Right $ VInt $ -int
    _ -> throwError "Wrong type for negative opearation"
  Not _ expr -> case transExpr expr of
    Right (VBool True) -> Right $ VBool False
    Right (VBool False) -> Right $ VBool True
    _ -> throwError "Wrong type for not opearation"
  EMul _ expr1 mulop expr2 -> transMulOp mulop expr1 expr2
  EAdd _ a op b -> transAddOp op a b
  ERel _ expr1 relop expr2 -> Right $ transRelOp relop expr1 expr2
  EAnd _ expr1 expr2 -> case transExpr expr1 of
    Right (VBool True) -> case transExpr expr2 of
      Right (VBool True) -> Right $ VBool True
      _ -> Right $ VBool False
    _ -> Right $ VBool False
  EOr _ expr1 expr2 -> case transExpr expr1 of
    Right (VBool True) -> Right $ VBool True
    _ -> case transExpr expr2 of
      Right (VBool True) -> Right $ VBool True
      _ -> Right $ VBool True
  _ -> Right $ VString "aaa"
  {-EVar _ ident -> do
    (var, loc, _) <- ask
    l <- Map.lookup ident var
    case l of
      Nothing -> throwError "Not existing variable used"
      Just lo -> do
        x <- Map.lookup l loc
        case x of
          Nothing -> throwError "Not existing variable used"
          Just xv -> xv 
  EApp _ ident exprs -> Nothing  -}

transAddOp :: AddOp -> Expr -> Expr -> Either [Char] Value
transAddOp x a b = case transExpr a of
  Right (VInt av) -> case transExpr b of
    Right (VInt bv) -> case x of
      Plus _ -> Right $ VInt $ av + bv
      Minus _ -> Right  $VInt $ av - bv
    _ -> throwError umt
  Right (VString as) -> case transExpr b of
    Right (VString bs) -> case x of
      Plus _ -> Right $ VString $ as ++ bs
      _ -> throwError "Not allowed opeartion on strings: -"
    _ -> throwError umt
  _ -> throwError $ "Using bool in opeartion" ++ show x

transMulOp :: MulOp -> Expr -> Expr -> Either [Char] Value
transMulOp x a b = case transExpr a of
  Right (VInt av) -> case transExpr b of
    Right (VInt 0) -> case x of
      Times _ -> Right $ VInt av
      Div p -> throwError $ "Not allowed opearation div with integer 0" ++ show p
      Mod p -> throwError $ "Not allowed opearation mod with integer 0" ++ show p
    Right (VInt bv) -> case x of
      Times _ -> Right $ VInt $ av * bv
      Div _ -> Right $ VInt $ div av bv
      Mod _ -> Right $ VInt $ mod av bv
    _ -> throwError umt
  _ -> throwError "Wrong variable type for operation"

transRelOp :: RelOp -> Expr -> Expr -> Value
transRelOp x a b = case transExpr a of
  Right (VInt va) -> case transExpr b of
    Right (VInt vb) -> case x of
      LTH _ -> VBool $ va < vb
      LE _ -> VBool $  va <= vb
      GTH _ -> VBool $  va > vb
      GE _ -> VBool $  va >=  vb
      EQU _ -> VBool $  va ==  vb
      NE _ -> VBool $  va /=  vb
    _ -> VBool False
  Right (VString sa) -> case transExpr b of
    Right (VString sb) -> case x of
      LTH _ -> VBool $  sa <  sb
      LE _ -> VBool $  sa <= sb
      GTH _ -> VBool $  sa > sb
      GE _ -> VBool $  sa >=  sb
      EQU _ -> VBool $  sa ==  sb
      NE _ -> VBool $  sa /=  sb
    _ -> VBool False
  _ -> VBool False