from enum import Enum
from dataclasses import dataclass
from typing import List, Union

class ExprType(Enum):
    VAR = 1
    INT_LIT = 2
    STRING_LIT = 3
    APP = 4
    LAMBDA = 5
    LET = 6

@dataclass
class Expr:
    type: ExprType
    value: Union[str, int, None] = None
    children: List['Expr'] = None

@dataclass
class Decl:
    name: str
    params: List[str]
    body: Expr

@dataclass
class Script:
    declarations: List[Decl]

class Parser:
    def __init__(self, input: str):
        self.input = input
        self.pos = 0

    def parse(self) -> Script:
        script = Script(declarations=[])
        while self.pos < len(self.input):
            script.declarations.append(self.parse_decl())
        return script

    def parse_decl(self) -> Decl:
        name = self.parse_identifier()
        params = []
        while self.peek() != '=':
            params.append(self.parse_identifier())
        self.consume('=')
        body = self.parse_expr()
        self.consume(';')
        return Decl(name, params, body)

    def parse_expr(self) -> Expr:
        if self.peek().isalpha():
            return Expr(ExprType.VAR, value=self.parse_identifier())
        raise ValueError("Unsupported expression")

    def parse_identifier(self) -> str:
        ident = ""
        while self.pos < len(self.input) and self.input[self.pos].isalpha():
            ident += self.input[self.pos]
            self.pos += 1
        if not ident:
            raise ValueError("Expected identifier")
        return ident

    def peek(self) -> str:
        return self.input[self.pos] if self.pos < len(self.input) else ''

    def consume(self, expected: str):
        if self.peek() != expected:
            raise ValueError(f"Expected '{expected}', got '{self.peek()}'")
        self.pos += 1