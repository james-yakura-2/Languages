type exp=
| True
| False
| If of exp * exp * exp
| Num of int
| IsZero of exp
| Plus of exp * exp
| Mult of exp * exp

exception Eval_error

let rec string_of_exp (expr:exp)=
match expr with
  True -> "true"
| False -> "false"
| If (e0,e1,e2) -> "if " ^ string_of_exp e0 ^ " then " ^ string_of_exp e1 ^ " else " ^ string_of_exp e2
| Num (n) -> string_of_int n
| IsZero (e) -> "(isZero " ^ string_of_exp e ^ ")"
| Plus (e0,e1) -> "(" ^ string_of_exp e0 ^ " + " ^ string_of_exp e1 ^ ")"
| Mult (e0,e1) -> "(" ^ string_of_exp e0 ^ " * " ^ string_of_exp e1 ^ ")"
| _ -> raise Eval_error

and eval (expr:exp) =
match expr with
  True -> True
| False -> False
| If (cond, _then, _else) ->
 ( match eval cond with
  | True -> eval _then
  | False -> eval _else
  | _ -> raise Eval_error)
| Num (n) -> Num (n)
| IsZero (value) -> (
  match eval value with
  | Num (n) ->
    (match n with
    | 0 -> True
    | n -> False
    | _ -> raise Eval_error)
  | _ -> raise Eval_error )
| Plus (left, right) ->
  (match eval left with
  | Num (left_n) ->
    (match eval right with
    | Num (right_n) -> Num (left_n + right_n)
    | _ -> raise Eval_error)
  | _ -> raise Eval_error)
| Mult (left, right) ->
  (match eval left with
  | Num (left_n) ->
    (match eval right with
    | Num (right_n) -> Num (left_n * right_n)
    | _ -> raise Eval_error)
  | _ -> raise Eval_error)
| _ -> raise Eval_error