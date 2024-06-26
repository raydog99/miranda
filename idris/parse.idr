module Miranda

import Data.String
import Control.Monad.State
import Control.Monad.Error.Either

data ExprType = VarExpr | IntLitExpr | StringLitExpr | AppExpr | LambdaExpr | LetExpr

data Expr : Type where
  Var : String -> Expr
  IntLit : Int -> Expr
  StringLit : String -> Expr
  App : Expr -> Expr -> Expr
  Lambda : List String -> Expr -> Expr
  Let : List (String, Expr) -> Expr -> Expr

record Decl where
  constructor MkDecl
  name : String
  params : List String
  body : Expr

record Script where
  constructor MkScript
  declarations : List Decl

Parser : Type -> Type
Parser a = StateT String (Either String) a

runParser : Parser a -> String -> Either String (a, String)
runParser p input = runStateT p input

parse : Parser Script
parse = do
  decls <- many parseDecl
  pure $ MkScript decls

parseDecl : Parser Decl
parseDecl = do
  name <- parseIdentifier
  params <- many parseIdentifier
  token "="
  body <- parseExpr
  token ";"
  pure $ MkDecl name params body

parseExpr : Parser Expr
parseExpr = parseVar

parseVar : Parser Expr
parseVar = Var <$> parseIdentifier

parseIdentifier : Parser String
parseIdentifier = do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  pure $ pack (c :: cs)

token : String -> Parser ()
token s = do
  _ <- string s
  skipSpaces

satisfy : (Char -> Bool) -> Parser Char
satisfy p = do
  input <- get
  case unpack input of
    [] => lift $ Left "Unexpected end of input"
    (c :: cs) => if p c
                 then do put (pack cs)
                         pure c
                 else lift $ Left "Unexpected character"

skipSpaces : Parser ()
skipSpaces = do
  _ <- many $ satisfy isSpace
  pure ()