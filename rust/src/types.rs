use std::collections::HashMap;
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
enum MirandaType {
    Num,
    Bool,
    Char,
    List(Box<MirandaType>),
    Tuple(Vec<MirandaType>),
    Function(Box<MirandaType>, Box<MirandaType>),
    TypeVar(String),
    UserDefined(String),
}

impl fmt::Display for MirandaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MirandaType::Num => write!(f, "num"),
            MirandaType::Bool => write!(f, "bool"),
            MirandaType::Char => write!(f, "char"),
            MirandaType::List(t) => write!(f, "[{}]", t),
            MirandaType::Tuple(ts) => write!(f, "({})", ts.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(",")),
            MirandaType::Function(p, r) => write!(f, "{} -> {}", p, r),
            MirandaType::TypeVar(v) => write!(f, "{}", v),
            MirandaType::UserDefined(n) => write!(f, "{}", n),
        }
    }
}

type TypeEnv = HashMap<String, MirandaType>;

enum Expr {
    Literal(Literal),
    Var(String),
    Lambda(String, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    ListExpr(Vec<Expr>),
    TupleExpr(Vec<Expr>),
}

enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug)]
struct TypeError(String);

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type error: {}", self.0)
    }
}

fn fresh_type_var(counter: &mut i32) -> MirandaType {
    *counter += 1;
    MirandaType::TypeVar(format!("*{}", counter))
}

fn infer_type(env: &TypeEnv, expr: &Expr, counter: &mut i32) -> Result<MirandaType, TypeError> {
    match expr {
        Expr::Literal(lit) => Ok(infer_literal_type(lit)),
        Expr::Var(name) => env.get(name).cloned().ok_or_else(|| TypeError(format!("Unbound variable: {}", name))),
        Expr::Lambda(param, body) => {
            let param_type = fresh_type_var(counter);
            let mut new_env = env.clone();
            new_env.insert(param.clone(), param_type.clone());
            let body_type = infer_type(&new_env, body, counter)?;
            Ok(MirandaType::Function(Box::new(param_type), Box::new(body_type)))
        }
        Expr::Apply(func, arg) => {
            let func_type = infer_type(env, func, counter)?;
            let arg_type = infer_type(env, arg, counter)?;
            match func_type {
                MirandaType::Function(param_type, return_type) => {
                    if *param_type == arg_type {
                        Ok(*return_type)
                    } else {
                        Err(TypeError("Function application type mismatch".to_string()))
                    }
                }
                _ => Err(TypeError("Cannot apply non-function type".to_string())),
            }
        }
        Expr::Let(name, value, body) => {
            let value_type = infer_type(env, value, counter)?;
            let mut new_env = env.clone();
            new_env.insert(name.clone(), value_type);
            infer_type(&new_env, body, counter)
        }
        Expr::ListExpr(elements) => {
            if elements.is_empty() {
                return Err(TypeError("Cannot infer type of empty list".to_string()));
            }
            let elem_type = infer_type(env, &elements[0], counter)?;
            for elem in &elements[1..] {
                let t = infer_type(env, elem, counter)?;
                if t != elem_type {
                    return Err(TypeError("Inconsistent types in list".to_string()));
                }
            }
            Ok(MirandaType::List(Box::new(elem_type)))
        }
        Expr::TupleExpr(elements) => {
            let mut types = Vec::new();
            for elem in elements {
                types.push(infer_type(env, elem, counter)?);
            }
            Ok(MirandaType::Tuple(types))
        }
    }
}

fn infer_literal_type(lit: &Literal) -> MirandaType {
    match lit {
        Literal::Int(_) | Literal::Float(_) => MirandaType::Num,
        Literal::Bool(_) => MirandaType::Bool,
        Literal::Char(_) => MirandaType::Char,
        Literal::String(_) => MirandaType::List(Box::new(MirandaType::Char)),
    }
}

fn type_check(env: &TypeEnv, expr: &Expr, expected_type: &MirandaType) -> Result<(), TypeError> {
    let mut counter = 0;
    let inferred_type = infer_type(env, expr, &mut counter)?;
    if inferred_type == *expected_type {
        Ok(())
    } else {
        Err(TypeError(format!("Type mismatch: expected {}, got {}", expected_type, inferred_type)))
    }
}