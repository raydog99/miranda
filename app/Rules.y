-- Grammar spec for the Happy parser generator

{
module MirandaParser where

data Token
    = Identifier String
    | IDENTIFIER String
    | Numeral Integer
    | FloatNum Double
    | CharConst Char
    | StringConst String
    | TypeVar String
    | Delimiter String
    | InfixOp String
    | PrefixOp String
    | Abstype
    | If
    | Otherwise
    | ReadVals
    | Show
    | Type
    | Where
    | With
    | Export
    | Free
    | Include
    | Insert
    | List
    | NoList
    | Eq
    | EqEq
    | ColonEq
    | DoubleColon
    | ThinArrow
    | VerticalBar
    | DoubleSlash
    | RightArrow
    | Semicolon
    | Comma
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace
    | LeftArrow
    | DoubleDot
    | DollarDollar
    | DollarMinus
    | DollarPlus
    | DollarStar
    | DollarIdentifier String
    | DollarIDENTIFIER String
    | VerticalBarBar
    | NewLine
    | Tab
    | FormFeed
    | Space
    | Comment String
    deriving (Show, Eq)
}

%name MirandaParser
%tokentype { Token }
%error { parseError }
%token
    | Identifier { Identifier $$ }
    | IDENTIFIER { IDENTIFIER $$ }
    | Numeral { Numeral $$ }
    | FloatNum { FloatNum $$ }
    | CharConst { CharConst $$ }
    | StringConst { StringConst $$ }
    | TypeVar { TypeVar $$ }
    | Delimiter { Delimiter $$ }
    | InfixOp { InfixOp $$ }
    | PrefixOp { PrefixOp $$ }
    | Abstype { Abstype }
    | If { If }
    | Otherwise { Otherwise }
    | ReadVals { ReadVals }
    | Show { Show }
    | Type { Type }
    | Where { Where }
    | With { With }
    | Export { Export }
    | Free { Free }
    | Include { Include }
    | Insert { Insert }
    | List { List }
    | NoList { NoList }
    | Eq { Eq }
    | EqEq { EqEq }
    | ColonEq { ColonEq }
    | DoubleColon { DoubleColon }
    | ThinArrow { ThinArrow }
    | VerticalBar { VerticalBar }
    | DoubleSlash { DoubleSlash }
    | RightArrow { RightArrow }
    | Semicolon { Semicolon }
    | Comma { Comma }
    | LeftParen { LeftParen }
    | RightParen { RightParen }
    | LeftBracket { LeftBracket }
    | RightBracket { RightBracket }
    | LeftBrace { LeftBrace }
    | RightBrace { RightBrace }
    | LeftArrow { LeftArrow }
    | DoubleDot { DoubleDot }
    | DollarDollar { DollarDollar }
    | DollarMinus { DollarMinus }
    | DollarPlus { DollarPlus }
    | DollarStar { DollarStar }
    | DollarIdentifier { DollarIdentifier $$ }
    | DollarIDENTIFIER { DollarIDENTIFIER $$ }
    | VerticalBarBar { VerticalBarBar }
    | NewLine { NewLine }
    | Tab { Tab }
    | FormFeed { FormFeed }
    | Space { Space }
    | Comment { Comment $$ }

%left '+' '-'
%left '*' '/'
%left '^'
%right '!'
%right '$' '-'
%nonassoc '.' ':'
%nonassoc '==' '~=' '<=' '<' '>' '>='
%nonassoc '->' ';' ','
%nonassoc '|'
%nonassoc '++' '--' '\/' '&' '>'
%nonassoc '=='
%nonassoc ':=' '::=' '::' '=>'
%nonassoc '$$' '$-' '$+'
%nonassoc '[' ']'
%nonassoc '{' '}'
%nonassoc '<-' '..' '//'