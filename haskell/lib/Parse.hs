module Miranda (parseMiranda) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

data Expr = Var String
          | IntLit Int
          | StringLit String
          | App Expr Expr
          | Lambda [String] Expr
          | Let [(String, Expr)] Expr
          deriving (Show)

data Decl = FunDef String [String] Expr
          deriving (Show)

type Script = [Decl]

lexer = T.makeTokenParser emptyDef

identifier = T.identifier lexer
reserved = T.reserved lexer
integer = T.integer lexer
stringLiteral = T.stringLiteral lexer
parens = T.parens lexer
semi = T.semi lexer

expr :: Parser Expr
expr = E.buildExpressionParser table term

term = parens expr
    <|> Var <$> identifier
    <|> IntLit . fromIntegral <$> integer
    <|> StringLit <$> stringLiteral
    <|> lambda
    <|> letExpr

table = [[E.Infix (pure App) E.AssocLeft]]

lambda = Lambda <$> (char '\\' *> many1 identifier) <*> (reserved "->" *> expr)

letExpr = Let <$> (reserved "let" *> many1 binding) <*> (reserved "in" *> expr)
  where binding = (,) <$> identifier <*> (reserved "=" *> expr)

funDef :: Parser Decl
funDef = FunDef <$> identifier <*> many identifier <*> (reserved "=" *> expr)