type miranda_type =
  | Num
  | Bool
  | Char
  | List of miranda_type
  | Tuple of miranda_type list
  | Function of miranda_type * miranda_type
  | TypeVar of string
  | UserDefined of string

type type_environment = (string * miranda_type) list

let rec infer_type env expr =
  match expr with
  | Literal l -> infer_literal_type l
  | Var name -> lookup_type env name
  | Lambda (param, body) ->
      let param_type = TypeVar (fresh_type_var ()) in
      let new_env = (param, param_type) :: env in
      let body_type = infer_type new_env body in
      Function (param_type, body_type)
  | Apply (func, arg) ->
      let func_type = infer_type env func in
      let arg_type = infer_type env arg in
      match func_type with
      | Function (param_type, return_type) ->
          if type_equal param_type arg_type then
            return_type
          else
            raise (Type_error "Function application type mismatch")
      | _ -> raise (Type_error "Cannot apply non-function type")
  | Let (name, value, body) ->
      let value_type = infer_type env value in
      let new_env = (name, value_type) :: env in
      infer_type new_env body
  | List elements ->
      let elem_types = List.map (infer_type env) elements in
      let common_type = unify_list elem_types in
      List common_type
  | Tuple elements ->
      let elem_types = List.map (infer_type env) elements in
      Tuple elem_types

and infer_literal_type = function
  | Int _ -> Num
  | Float _ -> Num
  | Bool _ -> Bool
  | Char _ -> Char
  | String _ -> List Char

and lookup_type env name =
  match List.assoc_opt name env with
  | Some t -> t
  | None -> raise (Type_error ("Unbound variable: " ^ name))

and fresh_type_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "*" ^ string_of_int !counter

and unify_list types =
  match types with
  | [] -> raise (Type_error "Cannot infer type of empty list")
  | [t] -> t
  | t :: ts ->
      List.fold_left
        (fun acc t' ->
          if type_equal acc t' then acc
          else raise (Type_error "Inconsistent types in list"))
        t ts

and type_equal t1 t2 =
  match (t1, t2) with
  | (Num, Num) | (Bool, Bool) | (Char, Char) -> true
  | (List t1', List t2') -> type_equal t1' t2'
  | (Tuple ts1, Tuple ts2) ->
      List.length ts1 = List.length ts2 &&
      List.for_all2 type_equal ts1 ts2
  | (Function (p1, r1), Function (p2, r2)) ->
      type_equal p1 p2 && type_equal r1 r2
  | (TypeVar v1, TypeVar v2) -> v1 = v2
  | (UserDefined n1, UserDefined n2) -> n1 = n2
  | _ -> false

let type_check env expr expected_type =
  let inferred_type = infer_type env expr in
  if type_equal inferred_type expected_type then
    ()
  else
    raise (Type_error "Type mismatch")

let rec string_of_miranda_type = function
  | Num -> "num"
  | Bool -> "bool"
  | Char -> "char"
  | List t -> "[" ^ string_of_miranda_type t ^ "]"
  | Tuple ts ->
      "(" ^ String.concat "," (List.map string_of_miranda_type ts) ^ ")"
  | Function (t1, t2) ->
      string_of_miranda_type t1 ^ " -> " ^ string_of_miranda_type t2
  | TypeVar v -> v
  | UserDefined name -> name