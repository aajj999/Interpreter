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
import Distribution.Compat.Lens (_1, view)

type Location = Int
data Value = VInt Integer | VBool Bool | VString String deriving Show
type Variables = Map.Map Ident Location
type Locations = Map.Map Location Value

data Function = VFunc Type [Arg] Block
type Functions = Map.Map Ident Function

type Env = (Variables, Locations, Functions)

data Err =
  UnmatchingTypes BNFC'Position |
  NotExisting Ident BNFC'Position |
  NegOp BNFC'Position |
  NotAllowed BNFC'Position |
  NotOp BNFC'Position |
  Div0 BNFC'Position |
  WrongArgs BNFC'Position |
  NotType Type BNFC'Position 

instance Show Err where
  show (UnmatchingTypes p) = "Unmatching types in operation " ++ show p
  show (NotExisting id p) = "Not existing ident used " ++ show id ++ " " ++ show p
  show (NegOp p) = "Wrong type for negative opearation " ++ show p
  show (NotAllowed p) = "Not allowed operation on strings: - " ++ show p
  show (NotOp p) = "Wrong type for not opearation " ++ show p
  show (Div0 p) = "Not allowed opearation div with integer 0 " ++ show p
  show (WrongArgs p) = "Wrong amount of arguments for the function " ++ show p
  show (NotType t p) = "Wrong type " ++ show t ++ " " ++ show p

type ErrM = Either Err

env :: Variables -> Locations -> Functions -> Env
env var loc fun = (var, loc, fun)

newLoc :: Locations -> Location
newLoc l = if Map.null l then 1 else (do
           let p = foldr1 (\x y ->if x >= y then x else y) $ Map.keys l
           p + 1)

interpret :: Program -> IO()
interpret (Program _ globals topdefs) = do
  let e = execState (transGlobals globals) (env Map.empty Map.empty Map.empty)
  let (result, (var, loc, fun)) = runState (transTopDefs topdefs) e
  res <- runExceptT result
  case res of
    Right () -> putStr "uuu"
    Left err -> print err
  let ret = Map.lookup 0 loc
  case ret of
    Just (VInt r) -> do
      putStrLn $ "Main result: " ++ show r
    _ -> putStrLn "Main result: 0"

transGlobals :: [Global] -> State Env (ErrM String)
transGlobals [] = return $ Right ""
transGlobals (x : xs) = do
  transGlobal x
  transGlobals xs

transGlobal :: Global -> State Env (ErrM String)
transGlobal x = case x of
  GlobalDef _ t id expr -> do
    e <- get
    let (val, (var, loc, fun)) = runState (transExpr expr) e
    case val of
      Left s -> return $ Left s
      Right val -> do
        let l = newLoc loc
        let var' = Map.insert id l var
        let loc' = Map.insert l val loc
        put (var', loc', fun)
        return $ Right ""

transTopDefs :: [TopDef] -> State Env (ExceptT Err IO ())
transTopDefs [] = return $ liftIO $ putStr ""
transTopDefs (x : xs) = do
  case x of
    FnDef _ t (Ident "main") args block -> do
      (var, loc, fun) <- get
      let (r, v) = runState (transTopDef x) (var, loc, fun)
      put v
      return r
    FnDef _ t id args block -> do
      (var, loc, fun) <- get
      let fun' = Map.insert id (VFunc t args block) fun
      put (var, loc, fun')
      return $ evalState (transTopDefs xs) (var, loc, fun')

transTopDef :: TopDef -> State Env (ExceptT Err IO())
transTopDef (FnDef p _ _  _ b) = do
  v <- get
  let (ret, v') = runState (transStmt $ BStmt p b) v
  put v'
  return ret

transBlock :: [Stmt] -> State Env(ExceptT Err IO ())
transBlock [] = return $ liftIO $ putStr ""
transBlock (s : ss) = do
  e <- get
  let (ret, (var, loc, fun)) = runState (transStmt s) e
  let (ret2, (_, loc', fun')) = runState (transBlock ss) (var, loc, fun)
  put(var, loc', fun')
  return do
    ret
    ret2

transStmt :: Stmt -> State Env (ExceptT Err IO())
transStmt x = case x of
  Empty _ -> return $ liftIO $ putStr ""
  BStmt _ block -> case block of
    Block p [] -> return $ liftIO $ putStr ""
    Block p ss -> do
      (var, loc, fun) <- get
      let (ret, (var', loc', fun')) =  runState (transBlock ss) (var, loc, fun)
      put (var, loc', fun')
      return ret
  Decl p t items -> case items of
    [] -> return $  liftIO $ putStr ""
    (i : is) -> do
      e <- get
      let e' = execState (transItem t i) e
      put $ execState (transStmt $ Decl p t is) e'
      (var, loc, fun) <- get
      return $ liftIO $ putStr ""
  Ass _ ident expr -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ liftIO $ putStr ""
      Just lo -> do
        case evalState (transExpr expr) (var, loc, fun) of
          Right x -> do
            let loc' = Map.insert lo x loc
            put (var, loc', fun)
            return $ liftIO $ putStr ""
          Left err -> return $ liftIO $ print err
  Incr _ ident -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ liftIO $ putStr ""
      Just lo -> do
        let val = Map.lookup lo loc
        case val of
          Just (VInt valv) -> do
            let loc' = Map.insert lo (VInt (valv + 1)) loc
            put (var, loc', fun)
            return $ liftIO $ putStr ""
          _ -> return $ throwError $ NotOp Nothing
  Decr _ ident -> do
    (var, loc, fun) <- get
    let l = Map.lookup ident var
    case l of
      Nothing -> return $ liftIO $ putStr ""
      Just lo -> do
        let val = Map.lookup lo loc
        case val of
          Just (VInt valv) -> do
            let loc' = Map.insert lo (VInt (valv - 1)) loc
            put (var, loc', fun)
            return $ liftIO $ putStr ""
          _ -> return $ liftIO $ putStr ""
  Ret _ expr -> do
    (var, loc, fun) <- get
    case evalState (transExpr expr) (var, loc, fun) of
      Right ret -> do
        let loc' = Map.insert 0 ret loc
        put(var, loc', fun)
        return $ liftIO $ putStr ""
      _ -> return $ liftIO $ putStr ""
  Cond _ expr stmt -> do
    e <- get
    case evalState (transExpr expr) e of
      Right (VBool True) -> do
        let (ret, (var, loc, fun)) = runState (transStmt stmt) e
        put (var, loc, fun)
        return ret
      _ -> return $ liftIO $ putStr ""
  CondElse _ expr stmt1 stmt2 -> do
    e <- get
    case evalState (transExpr expr) e of
      Right (VBool True) -> do
        let (ret, (var, loc, fun)) = runState (transStmt stmt1) e
        put (var, loc, fun)
        return ret
      Right (VBool False) -> do
        let (ret, (var, loc, fun)) = runState (transStmt stmt2) e
        put (var, loc, fun)
        return ret
      _ -> return $ liftIO $ putStr ""
  While _ expr stmt -> do
    e <- get
    case evalState (transExpr expr) e of
      Right (VBool True) -> do
        let(ret1, (var, loc, fun)) = runState (transStmt stmt) e
        let(ret2, (var', loc', fun')) = runState (transStmt x) (var, loc, fun)
        put(var', loc', fun')
        return $ do
          ret1
          ret2
      Right (VBool False) -> do
        let (var, loc, _) = e
        return $ liftIO $ putStr ""
      _ -> return $ liftIO $ putStr ""
  SExp _ expr -> do
    e <- get
    put $ execState (transExpr expr) e
    return $ liftIO $ putStr ""
  Print _ expr -> do
    e <- get
    case evalState (transExpr expr) e of
      Right (VString val) -> return $ liftIO $ print val
      Right (VInt val) -> return $ liftIO $ print val
      Right (VBool val) -> return $ liftIO $ print val
      _ -> return $ liftIO $ putStr ""

transItem :: Type -> Item -> State Env (ErrM String)
transItem t x = case x of
  NoInit _ ident -> do
    (var, loc, fun) <- get
    let l = newLoc loc
    let var' = Map.insert ident l var
    case t of
          Str _ -> do
            let loc' = Map.insert l (VString "") loc
            put (var', loc', fun)
            return $ Right ""
          Int _ -> do
            let loc' = Map.insert l (VInt 0)  loc
            put (var', loc', fun)
            return $ Right ""
          Bool _ -> do
            let loc' = Map.insert l (VBool False) loc
            put (var', loc', fun)
            return $ Right ""
          _ -> return $ throwError $ NotType t Nothing

  Init _ ident expr -> do
    (var, loc, fun) <- get
    let l = newLoc loc
    let var' = Map.insert ident l var
    case evalState (transExpr expr) (var, loc, fun) of
      Right val -> do
        let loc' = Map.insert l val loc
        put (var', loc', fun)
        return $ Right ""
      Left err -> return $ Left err

transExpr :: Expr -> State Env (ErrM Value)
transExpr x = do
  e <- get
  case x of
    AbsAJ.ELitInt _ integer -> return $ Right $ VInt integer
    AbsAJ.ELitTrue _ -> return $ Right $ VBool True
    AbsAJ.ELitFalse _ -> return $ Right $ VBool False
    AbsAJ.EString _ string -> return $ Right $ VString string
    Neg _ expr -> case evalState (transExpr expr) e of
      Right (VInt int) -> return $ Right $ VInt $ -int
      _ -> return $ throwError $ NegOp Nothing
    Not _ expr -> case evalState (transExpr expr) e of
      Right (VBool True) -> return $ Right $ VBool False
      Right (VBool False) -> return $ Right $ VBool True
      _ -> return $ throwError $ NotOp Nothing
    EMul _ expr1 mulop expr2 -> case evalState (transMulOp mulop expr1 expr2) e of
      Right v -> return $ Right v
      Left err -> return $ Left err
    EAdd _ a op b -> return $ evalState (transAddOp op a b) e
    ERel _ expr1 relop expr2 -> case evalState (transRelOp relop expr1 expr2) e of
      Right v -> return $ Right v
      Left err -> return $ Left $ err
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
    EVar p ident -> do
      (var, loc, _) <- get
      let l = Map.lookup ident var
      case l of
        Nothing -> return $ throwError $ NotExisting ident p
        Just lo -> do
          let value = Map.lookup lo loc
          case value of
            Nothing -> return $ throwError $ NotExisting ident p
            Just xv -> return $ Right xv
    EApp p ident exprs -> do
      (var, loc, fun) <- get
      case Map.lookup ident fun of
        Nothing -> return $ throwError $ NotExisting ident p
        Just (VFunc t args b) -> do
          let (var', loc', _) = execState (transArgs args exprs) (var, loc, fun)
          let (ret, (var'', loc'', fun'')) = runState (transStmt (BStmt p b)) (var', loc', fun)
          put(var, loc'', fun)
          return case Map.lookup 0 loc'' of
            Nothing -> do
              Right $ VInt 0
            Just v -> Right v

transArgs :: [Arg] -> [Expr] -> State Env (ErrM String)
transArgs [] [] = return $ Right ""
transArgs (a : as) (e : es) = do
  (var, loc, fun) <- get
  let (var', loc', fun) = execState (transArg a e) (var, loc, fun)
  put $ execState (transArgs as es) (var', loc', fun)
  return $ Right ""
transArgs _ _ = return $ throwError $ WrongArgs Nothing

transArg :: Arg -> Expr-> State Env ()
transArg a e = do
  (var, loc, fun) <- get
  let val = evalState (transExpr e) (var, loc, fun)
  case val of
    Right v -> do
      case a of
        Arg _ t id -> do
          let l = newLoc loc
          let var' = Map.insert id l var
          let loc' = Map.insert l v loc
          put (var', loc', fun)
          return ()
        InArg _ t id -> do
          let l = newLoc loc
          let var' = Map.insert id l var
          let loc' = Map.insert l v loc
          put (var', loc', fun)
          return ()
        OutArg _ t id -> do
          case e of
            EVar _ out -> do
              let l = Map.lookup out var
              case l of
                Just lo -> do
                  let var' = Map.insert id lo var
                  put (var', loc, fun)
                  return()
                _ -> return()
            _ -> return()
    _ -> return ()

transAddOp :: AddOp -> Expr -> Expr -> State Env (ErrM Value)
transAddOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt av) -> case evalState (transExpr b) e of
      Right (VInt bv) -> case x of
        Plus _ -> return $ Right $ VInt $ av + bv
        Minus _ -> return $ Right  $VInt $ av - bv
      _ -> return $ throwError $ UnmatchingTypes Nothing
    Right (VString as) -> case evalState (transExpr b) e of
      Right (VString bs) -> case x of
        Plus _ -> return $ Right $ VString $ as ++ bs
        _ -> return $ throwError $ NotAllowed Nothing
      _ -> return $ throwError $ UnmatchingTypes Nothing
    _ -> return $ throwError $ NotAllowed Nothing

transMulOp :: MulOp -> Expr -> Expr -> State Env (ErrM Value)
transMulOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt av) -> case evalState (transExpr b) e of
      Right (VInt 0) -> case x of
        Times _ -> return $ Right $ VInt av
        Div p -> return $ throwError $ Div0 p
        Mod p -> return $ throwError $ NotAllowed p
      Right (VInt bv) -> case x of
        Times _ -> return $ Right $ VInt $ av * bv
        Div _ -> return $ Right $ VInt $ div av bv
        Mod _ -> return $ Right $ VInt $ mod av bv
      _ -> return $ throwError $ UnmatchingTypes Nothing
    _ -> return $ throwError $ NotAllowed Nothing

transRelOp :: RelOp -> Expr -> Expr -> State Env (ErrM Value)
transRelOp x a b = do
  e <- get
  case evalState (transExpr a) e of
    Right (VInt va) -> case evalState (transExpr b) e of
      Right (VInt vb) -> case x of
        LTH _ -> return $ Right $ VBool $ va < vb
        LE _ -> return $ Right $ VBool $  va <= vb
        GTH _ -> return $ Right $ VBool $  va > vb
        GE _ -> return $ Right $ VBool $  va >=  vb
        EQU _ -> return $ Right $ VBool $  va ==  vb
        NE _ -> return $ Right $ VBool $  va /=  vb
      _ -> return $ Right $ VBool False
    Right (VString sa) -> case evalState (transExpr b) e of
      Right (VString sb) -> case x of
        LTH _ -> return $ Right $ VBool $  sa <  sb
        LE _ -> return $ Right $ VBool $  sa <= sb
        GTH _ -> return $ Right $ VBool $  sa > sb
        GE _ -> return $ Right $ VBool $  sa >=  sb
        EQU _ -> return $ Right $ VBool $  sa ==  sb
        NE _ -> return $ Right $ VBool $  sa /=  sb
      _ -> return $ Right $ VBool False
    _ -> return $ Right $ VBool False