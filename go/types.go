package miranda

import (
    "fmt"
    "strings"
)

type MirandaType interface {
    String() string
}

type Num struct{}
type Bool struct{}
type Char struct{}
type List struct{ ElemType MirandaType }
type Tuple struct{ ElemTypes []MirandaType }
type Function struct{ ParamType, ReturnType MirandaType }
type TypeVar struct{ Name string }
type UserDefined struct{ Name string }

func (Num) String() string         { return "num" }
func (Bool) String() string        { return "bool" }
func (Char) String() string        { return "char" }
func (l List) String() string      { return "[" + l.ElemType.String() + "]" }
func (t Tuple) String() string     { return "(" + strings.Join(func() []string {
    s := make([]string, len(t.ElemTypes))
    for i, t := range t.ElemTypes {
        s[i] = t.String()
    }
    return s
}(), ",") + ")" }
func (f Function) String() string  { return f.ParamType.String() + " -> " + f.ReturnType.String() }
func (v TypeVar) String() string   { return v.Name }
func (u UserDefined) String() string { return u.Name }

type TypeEnv map[string]MirandaType

type Expr interface {
    inferType(TypeEnv) (MirandaType, error)
}

type Literal struct{ Value interface{} }
type Var struct{ Name string }
type Lambda struct{ Param string; Body Expr }
type Apply struct{ Func, Arg Expr }
type Let struct{ Name string; Value, Body Expr }
type ListExpr struct{ Elements []Expr }
type TupleExpr struct{ Elements []Expr }

func (l Literal) inferType(env TypeEnv) (MirandaType, error) {
    switch l.Value.(type) {
    case int, float64:
        return Num{}, nil
    case bool:
        return Bool{}, nil
    case rune:
        return Char{}, nil
    case string:
        return List{Char{}}, nil
    default:
        return nil, fmt.Errorf("unknown literal type")
    }
}

func (v Var) inferType(env TypeEnv) (MirandaType, error) {
    if t, ok := env[v.Name]; ok {
        return t, nil
    }
    return nil, fmt.Errorf("unbound variable: %s", v.Name)
}

func (l Lambda) inferType(env TypeEnv) (MirandaType, error) {
    paramType := TypeVar{fmt.Sprintf("*%d", len(env))}
    newEnv := make(TypeEnv)
    for k, v := range env {
        newEnv[k] = v
    }
    newEnv[l.Param] = paramType
    bodyType, err := l.Body.inferType(newEnv)
    if err != nil {
        return nil, err
    }
    return Function{paramType, bodyType}, nil
}

func (a Apply) inferType(env TypeEnv) (MirandaType, error) {
    funcType, err := a.Func.inferType(env)
    if err != nil {
        return nil, err
    }
    argType, err := a.Arg.inferType(env)
    if err != nil {
        return nil, err
    }
    if f, ok := funcType.(Function); ok {
        if f.ParamType == argType {
            return f.ReturnType, nil
        }
        return nil, fmt.Errorf("function application type mismatch")
    }
    return nil, fmt.Errorf("cannot apply non-function type")
}

func (l Let) inferType(env TypeEnv) (MirandaType, error) {
    valueType, err := l.Value.inferType(env)
    if err != nil {
        return nil, err
    }
    newEnv := make(TypeEnv)
    for k, v := range env {
        newEnv[k] = v
    }
    newEnv[l.Name] = valueType
    return l.Body.inferType(newEnv)
}

func (l ListExpr) inferType(env TypeEnv) (MirandaType, error) {
    if len(l.Elements) == 0 {
        return nil, fmt.Errorf("cannot infer type of empty list")
    }
    elemType, err := l.Elements[0].inferType(env)
    if err != nil {
        return nil, err
    }
    for _, elem := range l.Elements[1:] {
        t, err := elem.inferType(env)
        if err != nil {
            return nil, err
        }
        if t != elemType {
            return nil, fmt.Errorf("inconsistent types in list")
        }
    }
    return List{elemType}, nil
}

func (t TupleExpr) inferType(env TypeEnv) (MirandaType, error) {
    types := make([]MirandaType, len(t.Elements))
    for i, elem := range t.Elements {
        var err error
        types[i], err = elem.inferType(env)
        if err != nil {
            return nil, err
        }
    }
    return Tuple{types}, nil
}

func TypeCheck(env TypeEnv, expr Expr, expectedType MirandaType) error {
    inferredType, err := expr.inferType(env)
    if err != nil {
        return err
    }
    if inferredType != expectedType {
        return fmt.Errorf("type mismatch: expected %v, got %v", expectedType, inferredType)
    }
    return nil
}