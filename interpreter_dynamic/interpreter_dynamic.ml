type typ =  
| TBool  
| TInt  
| TArrow of typ * typ  

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp
  | TypeError

type type_environment = (string * typ) list
type environment = (string * exp) list

exception Eval_error 
exception Substitution_error  

let rec step (env : environment) (e : exp) : (environment * exp) = match e with
| If (cond, then_exp, else_exp) -> 
  (match multi_step env cond with
  | (en, cond_eval) -> 
    (match cond_eval with
      | True -> multi_step env then_exp
      | False -> multi_step env else_exp
      | _ -> (en, TypeError)
    )
  | _ -> raise Eval_error
  )
| IsZero (number) -> 
  (match multi_step env number with
  | (en, number_eval) -> 
    (match number_eval with
    | Num (value) ->
      (match value with 
        | 0 -> (en, True)
        | _ -> (en, False)
      )
    | _ -> (en, TypeError)
    )
  | _ -> raise Eval_error
  )
| Plus (left, right) ->
  (match multi_step env left with
  | (env0, Num(left_value)) ->
    (match multi_step env right with
    | (env1, Num(right_value)) -> (env1, Num(left_value+right_value))
    | _ -> (env, TypeError)
    )
  | _ -> (env, TypeError)
  )
| Mult (left, right) ->
  (match multi_step env left with
  | (env0, Num(left_value)) ->
    (match multi_step env right with
    | (env1, Num(right_value)) -> (env1, Num(left_value*right_value))
    | _ -> (env, TypeError)
    )
  | _ -> (env, TypeError)
  )
| Apply (expression, input) ->
  (match multi_step env expression with
  | (en,Lambda (var, expr)) -> step en (Let(var, input, expr))
  | _ -> multi_step env expression
  )
| Var (name) -> multi_step env (lookup_variable env name)
| Let (var, input, expression) -> multi_step (update_environment [] env var input) expression
| _ -> raise Eval_error

and lookup_variable (env:environment) (name:string) : exp = match env with
| (current_name, current_value)::tail ->
  if current_name=name
    then current_value
  else lookup_variable tail name
| [] -> TypeError

and update_environment (env_h:environment) (env_t:environment) (name:string) (value:exp) : environment = match env_t with
| (current_name, current_value)::tail ->
  if current_name=name
    then
      match multi_step (env_h@env_t) value with
      | (en, value_eval) -> (name, value_eval)::env_h@env_t
      | _ -> raise Eval_error
  else update_environment ((current_name, current_value)::env_h) tail name value
| [] -> 
  match multi_step env_h value with
  | (en, value_eval) -> (name, value_eval)::env_h
  | _ -> raise Eval_error

and multi_step (env : environment) (e : exp) : (environment * exp) = match e with
| True -> (env, e)
| False -> (env, e)
| Num (value) -> (env, e) 
| Lambda(var, expr) -> (env,e)
| TypeError -> (env, e)
| _ -> step env e
