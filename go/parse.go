package miranda

import (
    "fmt"
    "strings"
    "unicode"
)

type ExprType int

const (
    VarExpr ExprType = iota
    IntLitExpr
    StringLitExpr
    AppExpr
    LambdaExpr
    LetExpr
)

type Expr struct {
    Type     ExprType
    Value    interface{}
    Children []*Expr
}

type Decl struct {
    Name   string
    Params []string
    Body   *Expr
}

type Script struct {
    Declarations []*Decl
}

type Parser struct {
    input string
    pos   int
}

func NewParser(input string) *Parser {
    return &Parser{input: input, pos: 0}
}

func (p *Parser) parse() (*Script, error) {
    script := &Script{}
    for p.pos < len(p.input) {
        decl, err := p.parseDecl()
        if err != nil {
            return nil, err
        }
        script.Declarations = append(script.Declarations, decl)
    }
    return script, nil
}

func (p *Parser) parseDecl() (*Decl, error) {
    name, err := p.parseIdentifier()
    if err != nil {
        return nil, err
    }
    params := []string{}
    for p.peek() != '=' {
        param, err := p.parseIdentifier()
        if err != nil {
            return nil, err
        }
        params = append(params, param)
    }
    p.consume('=')
    body, err := p.parseExpr()
    if err != nil {
        return nil, err
    }
    p.consume(';')
    return &Decl{Name: name, Params: params, Body: body}, nil
}

func (p *Parser) parseExpr() (*Expr, error) {
    // Simplified expression parsing
    if unicode.IsLetter(rune(p.peek())) {
        ident, err := p.parseIdentifier()
        if err != nil {
            return nil, err
        }
        return &Expr{Type: VarExpr, Value: ident}, nil
    }
    return nil, fmt.Errorf("unsupported expression")
}

func (p *Parser) parseIdentifier() (string, error) {
    start := p.pos
    for p.pos < len(p.input) && unicode.IsLetter(rune(p.input[p.pos])) {
        p.pos++
    }
    if start == p.pos {
        return "", fmt.Errorf("expected identifier")
    }
    return p.input[start:p.pos], nil
}

func (p *Parser) peek() byte {
    if p.pos >= len(p.input) {
        return 0
    }
    return p.input[p.pos]
}

func (p *Parser) consume(expected byte) error {
    if p.peek() != expected {
        return fmt.Errorf("expected '%c', got '%c'", expected, p.peek())
    }
    p.pos++
    return nil
}