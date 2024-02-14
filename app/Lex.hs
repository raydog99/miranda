import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

data Token = Identifier String
           | UppercaseIdentifier String
           | Numeral Integer
           | FloatNum Double
           | CharConst Char
           | StringConst String
           | TypeVar String
           | Delimiter String
           | Layout String
           deriving (Show, Eq)

mirandaDef :: LanguageDef st
mirandaDef = haskellDef
    { identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_'"
    , reservedOpNames = ["++", "--", ":", "\\/", "&", ">", ">=", "=", "~=", "<=", "<", "+", "*", "/", "div", "mod", "^", ".", "!", "$"]
    , reservedNames   = ["abstype", "if", "otherwise", "readvals", "show", "type", "where", "with", "%export", "%free", "%include", "%insert", "%list", "%nolist", "=", "==", "::=", "::", "=>", "|", "//", "->", ";", ",", "(", ")", "[", "]", "{", "}", "<-", "..", "$$", "$-", "$+"]
    }

lexer :: TokenParser st
lexer = makeTokenParser mirandaDef

layout :: Parser Token
layout = try (string "||" >> many (noneOf "\n") >> newline >> return (Layout ""))
     <?> "layout"

comment :: Parser Token
comment = try (string "||" >> many (noneOf "\n") >> newline >> return (Layout ""))
      <?> "comment"

identifier :: Parser Token
identifier = do
    name <- identifier lexer
    if name `elem` reservedNames mirandaDef
        then unexpected ("reserved word " ++ show name)
        else return $ Identifier name

uppercaseIdentifier :: Parser Token
uppercaseIdentifier = do
    name <- reserved lexer
    return $ UppercaseIdentifier name

numeral :: Parser Token
numeral = do
    n <- natural lexer
    return $ Numeral n

floatNum :: Parser Token
floatNum = do
    f <- float lexer
    return $ FloatNum f

charConst :: Parser Token
charConst = do
    c <- charLiteral lexer
    return $ CharConst c

stringConst :: Parser Token
stringConst = do
    s <- stringLiteral lexer
    return $ StringConst s

typeVar :: Parser Token
typeVar = do
    tv <- try (string "**" <|> string "*") -- match longest string first
    return $ TypeVar tv

delimiter :: Parser Token
delimiter = do
    delim <- operator lexer
    return $ Delimiter delim

token :: Parser Token
token = layout <|> comment <|> identifier <|> uppercaseIdentifier <|> numeral <|> floatNum <|> charConst <|> stringConst <|> typeVar <|> delimiter

mirandaLexer :: Parser [Token]
mirandaLexer = many token