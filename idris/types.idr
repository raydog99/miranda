module MirandaTypeChecker

import Data.SortedMap

%default total

data MirandaType : Type where
  Num : MirandaType
  Bool : MirandaType
  Char : MirandaType
  List : MirandaType -> MirandaType
  Tuple : List MirandaType -> MirandaType
  Function : MirandaType -> MirandaType -> MirandaType
  TypeVar : String -> MirandaType
  UserDefined : String -> MirandaType

Show MirandaType where
  show Num = "num"
  show Bool = "bool"
  show Char = "char"
  show (List t) = "[" ++ show t ++ "]"
  show (Tuple ts) = "(" ++ (concat $ intersperse "," $ map show ts) ++ ")"
  show (Function p r) = show p ++ " -> " ++ show r
  show (TypeVar n) = n
  show (UserDefined n) = n

Eq MirandaType where
  Num == Num = True
  Bool == Bool = True
  Char == Char = True
  (List a) == (List b) = a == b
  (Tuple as) == (Tuple bs) = as == bs
  (Function p1 r1) == (Function p2 r2) = p1 == p2 && r1 == r2
  (TypeVar a) == (TypeVar b) = a == b
  (UserDefined a) == (UserDefined b) = a == b
  _ == _ = False

TypeEnv : Type
TypeEnv = SortedMap String MirandaType

data Expr : Type where
  Literal : (value : String) -> Expr
  Var : (name : String) -> Expr
  Lambda : (param : String) -> (body : Expr) -> Expr
  Apply : (func : Expr) -> (arg : Expr) -> Expr
  Let : (name : String) -> (value : Expr) -> (body : Expr) -> Expr
  ListExpr : (elements : List Expr) -> Expr
  TupleExpr : (elements : List Expr) -> Expr

data TypeError : Type where
  UnboundVariable : (name : String) -> TypeError
  TypeMismatch : (expected : MirandaType) -> (got : MirandaType) -> TypeError
  CannotApplyNonFunction : TypeError
  InconsistentListTypes : TypeError
  CannotInferEmptyList : TypeError

Show TypeError where
  show (UnboundVariable name) = "Unbound variable: " ++ name
  show (TypeMismatch expected got) = "Type mismatch: expected " ++ show expected ++ ", got " ++ show got
  show CannotApplyNonFunction = "Cannot apply non-function type"
  show InconsistentListTypes = "Inconsistent types in list"
  show CannotInferEmptyList = "Cannot infer type of empty list"

inferLiteralType : String -> MirandaType
inferLiteralType v =
  case v of
    _ => if all isDigit (unpack v) then Num
         else if v == "True" || v == "False" then Bool
         else if length v == 1 then Char
         else List Char

inferType : TypeEnv -> Expr -> Either TypeError MirandaType
inferType env (Literal v) = Right $ inferLiteralType v
inferType env (Var name) =
  case lookup name env of
    Just t => Right t
    Nothing => Left $ UnboundVariable name
inferType env (Lambda param body) =
  do let paramType = TypeVar "*"
     bodyType <- inferType (insert param paramType env) body
     pure $ Function paramType bodyType
inferType env (Apply func arg) =
  do funcType <- inferType env func
     argType <- inferType env arg
     case funcType of
       Function paramType returnType =>
         if paramType == argType
           then Right returnType
           else Left $ TypeMismatch paramType argType
       _ => Left CannotApplyNonFunction
inferType env (Let name value body) =
  do valueType <- inferType env value
     inferType (insert name valueType env) body
inferType env (ListExpr []) = Left CannotInferEmptyList
inferType env (ListExpr (x :: xs)) =
  do elemType <- inferType env x
     forM_ xs $ \elem => do
       t <- inferType env elem
       if t == elemType
         then Right ()
         else Left InconsistentListTypes
     pure $ List elemType
inferType env (TupleExpr elements) =
  do types <- traverse (inferType env) elements
     pure $ Tuple types

typeCheck : TypeEnv -> Expr -> MirandaType -> Either TypeError ()
typeCheck env expr expectedType =
  do inferredType <- inferType env expr
     if inferredType == expectedType
       then Right ()
       else Left $ TypeMismatch expectedType inferredType