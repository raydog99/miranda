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


%%
Script  : Decls { Script $1 }
Decls   : Decl Decls { $1 : $2 }
        | { [] }

Decl    : Def { [$1] }
        | TDef { [$1] }
        | Spec { [$1] }
        | LibDir { [$1] }

Def     : FnForm '=' Rhs { Def $1 $3 }
        | Pat '=' Rhs { Def $1 $3 }

FnForm  : Var Formals { FnForm $1 $2 }
        | Pat '$' Var Pat { FnForm $1 [VarPat $3] }

TDef    : TForm '::=' Type { TDef $1 $3 }
        | 'abstype' TFormList 'with' Sig { TDefAbsType $2 $4 }

TForm   : Typename TypeVarList { TForm $1 $2 }
        | TypeVar '*' Typename TypeVarList { TFormAbstype $1 $3 $4 }

TypeVarList : { [] }
            | TypeVar TypeVarList { $1 : $2 }

Formals : Var FormalList { [$1] ++ $2 }
        | Pat '$' Var Pat FormalList { VarPat $3 : $5 }

FormalList : /* empty */ { [] }
           | Formal FormalList { $1 : $2 }

Formal  : Var { VarPat $1 }
        | Constructor { ConstrPat $1 }
        | Literal1 { LiteralPat $1 }
        | '(' PatList? ')' { TuplePat $2 }
        | '[' PatList? ']' { ListPat $2 }

PatList : Pat ',' PatList { $1 : $3 }
        | Pat { [$1] }

Rhs     : SimpleRhs ( ';' | 'cases' ) Cases { Rhs $1 $3 }
        | SimpleRhs ';' { SimpleRhs $1 [] }

Cases   : Alt (';' | '=' ) Cases { $1 : $3 }
        | LastCase { [$1] }

Alt     : Exp (',' 'if'? Exp) { Alt $1 (Just $3) }
        | Exp ',' 'otherwise' { Alt $1 Nothing }

LastCase : LastAlt Whdefs? { LastCase $1 $2 }

LastAlt : Exp (',' 'if'? Exp) { LastAlt $1 (Just $3) }
        | Exp ',' 'otherwise' { LastAlt $1 Nothing }

Whdefs  : 'where' Def Defs { [$2] ++ $3 }

Exp     : E1 ExpList? { buildExp $1 $2 }

ExpList : Exp ',' ExpList { $1 : $3 }
        | /* empty */ { [] }

E1      : Simple SimpleList { buildE1 $1 $2 }
        | Prefix1 E1 { PrefixE1 $1 $2 }
        | InfixE1 E1 { InfixE1 $1 $2 }

Simple  : Var { VarExp $1 }
        | Constructor { ConstrExp $1 }
        | Literal { LiteralExp $1 }
        | ReadVals { ReadValsExp }
        | Show { ShowExp }
        | '(' Infix1 E1 ')' { ParenExp $2 }
        | '(' E1 'infix' E1 ')' { InfixExp $2 $4 }

SimpleList : { [] }
           | Simple SimpleList { $1 : $2 }

Prefix1 : '~' { Prefix1 '~' }
        | '#' { Prefix1 '#' }

Infix1  : '++' | '--' | ':' | '\/' | '&' | '>' | '>=' | '=' | '~=' | '<=' | '<' | '+' | '*' | '/' | 'div' | 'mod' | '^' | '.' | '!' | '$identifier' | '$IDENTIFIER'

InfixE1 : Infix1 { Infix1Exp $1 }
        | '-' { Infix1Exp '-' }

Prefix : Prefix1 { Prefix1Exp $1 }
        | '-' { Prefix1Exp '-' }

InfixExp : E1 { Infix1Exp $1 }
          | InfixE1 E1 { Infix1E1Exp $1 $2 }

PrefixExp : E1 { Prefix1Exp $1 }
           | Prefix E1 { Prefix1E1Exp $1 $2 }

Literal : Numeral { NumeralLit $1 }
        | CharConst { CharConstLit $1 }
        | StringConst { StringConstLit $1 }

Literal1 : Literal { $1 }
         | FloatNum { FloatNumLit $1 }

TFormList : Typename TypeVarList { [$1] ++ $2 }
           | Typename TypeVarList '==' Type { [$1] ++ $2 ++ [$4] }
           | Typename TypeVarList '::=' Type { [$1] ++ $2 ++ [$4] }

Sig     : Spec SpecList { buildSig $1 $2 }

SpecList : Spec SpecList { $1 : $2 }
         | { [] }

Spec    : VarList '::' Type { VarListSpec $1 $3 }
        | TFormList '::' Type { TFormListSpec $1 $3 }

VarList : Var { [VarExp $1] }
        | VarList ',' VarList { VarExp $1 : $3 }

Comment : Comment { Comment $1 }