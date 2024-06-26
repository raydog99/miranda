open Angstrom

type identifier = string

type literal = 
  | IntLiteral of int
  | StringLiteral of string

type expression = 
  | Var of identifier
  | Lit of literal
  | App of expression * expression
  | Lambda of identifier list * expression
  | Let of (identifier * expression) list * expression

type declaration = 
  | FunctionDef of identifier * identifier list * expression

type script = declaration list

let is_lowercase c = 'a' <= c && c <= 'z'
let is_uppercase c = 'A' <= c && c <= 'Z'
let is_digit c = '0' <= c && c <= '9'
let is_alphanum c = is_lowercase c || is_uppercase c || is_digit c

let whitespace = take_while (function ' ' | '\t' | '\n' -> true | _ -> false)
let token p = p <* whitespace

let identifier = 
  let head = satisfy is_lowercase in
  let tail = take_while is_alphanum in
  token (lift2 (fun h t -> String.make 1 h ^ t) head tail)

let constructor = 
  let head = satisfy is_uppercase in
  let tail = take_while is_alphanum in
  token (lift2 (fun h t -> String.make 1 h ^ t) head tail)

let integer = token (take_while1 is_digit >>| int_of_string)
let string_literal = 
  token (char '"' *> take_while ((<>) '"') <* char '"' >>| (fun s -> StringLiteral s))

let rec expression input = 
  (simple_expression <|> lambda_expression) input

and simple_expression input =
  (choice [
    (identifier >>| fun id -> Var id);
    (integer >>| fun i -> Lit (IntLiteral i));
    string_literal >>| fun s -> Lit s;
    (char '(' *> expression <* char ')');
  ]) input

and lambda_expression input =
  (lift3
    (fun params _ body -> Lambda (params, body))
    (char '\\' *> sep_by1 identifier whitespace)
    (token (string "->"))
    expression) input

let let_expression =
  lift3
    (fun _ bindings body -> Let (bindings, body))
    (token (string "let"))
    (sep_by1 
      (lift3
        (fun id _ expr -> (id, expr))
        identifier
        (token (char '='))
        expression)
      (token (char ';')))
    (token (string "in") *> expression)

let function_def =
  lift4
    (fun name params _ body -> FunctionDef (name, params, body))
    identifier
    (many identifier)
    (token (char '='))
    expression