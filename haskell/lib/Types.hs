module MirandaTypeChecker where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

data MirandaType
  = Num
  | Bool
  | Char
  | List MirandaType
  | Tuple [MirandaType]
  | Function MirandaType MirandaType
  | TypeVar String
  | UserDefined String
  deriving (Eq, Show)

type TypeEnv = Map.Map String MirandaType

data Expr
  = Literal Literal
  | Var String
  | Lambda String Expr
  | Apply Expr Expr
  | Let String Expr Expr
  | ListExpr [Expr]
  | TupleExpr [Expr]

data Literal
  = IntLit Int
  | FloatLit Float
  | BoolLit Bool
  | CharLit Char
  | StringLit String

type TypeChecker a = StateT Int (ExceptT String IO) a

freshTypeVar :: TypeChecker MirandaType
freshTypeVar = do
  n <- get
  put (n + 1)
  return $ TypeVar ("*" ++ show n)

inferType :: TypeEnv -> Expr -> TypeChecker MirandaType
inferType env expr = case expr of
  Literal lit -> return $ inferLiteralType lit
  Var name -> case Map.lookup name env of
    Just t -> return t
    Nothing -> throwError $ "Unbound variable: " ++ name
  Lambda param body -> do
    paramType <- freshTypeVar
    bodyType <- inferType (Map.insert param paramType env) body
    return $ Function paramType bodyType
  Apply func arg -> do
    funcType <- inferType env func
    argType <- inferType env arg
    case funcType of
      Function paramType returnType ->
        if paramType == argType
          then return returnType
          else throwError "Function application type mismatch"
      _ -> throwError "Cannot apply non-function type"
  Let name value body -> do
    valueType <- inferType env value
    inferType (Map.insert name valueType env) body
  ListExpr elements -> do
    elemTypes <- mapM (inferType env) elements
    case elemTypes of
      [] -> throwError "Cannot infer type of empty list"
      (t:ts) -> if all (== t) ts
        then return $ List t
        else throwError "Inconsistent types in list"
  TupleExpr elements -> Tuple <$> mapM (inferType env) elements

inferLiteralType :: Literal -> MirandaType
inferLiteralType lit = case lit of
  IntLit _ -> Num
  FloatLit _ -> Num
  BoolLit _ -> Bool
  CharLit _ -> Char
  StringLit _ -> List Char

typeCheck :: TypeEnv -> Expr -> MirandaType -> TypeChecker ()
typeCheck env expr expectedType = do
  inferredType <- inferType env expr
  if inferredType == expectedType
    then return ()
    else throwError "Type mismatch"

runTypeChecker :: TypeChecker a -> IO (Either String a)
runTypeChecker checker = runExceptT (evalStateT checker 0)