open Ast
open Environment

let apply_iop op v1 v2 = IntV (op v1 v2) 

let apply_bop op v1 v2 = BoolV (op v1 v2) 

let apply_op op v1 v2 = 
  match op with
  | '+' -> apply_iop (+) v1 v2
  | '-' -> apply_iop (-) v1 v2
  | '=' -> apply_bop (=) v1 v2
  | '>' -> apply_bop (>) v1 v2
  | '<' -> apply_bop (<) v1 v2
  | _ -> failwith "invalid operator"

let rec value_of_expr : env -> expr -> value = fun env expr ->
  match expr with 
  | Int n  -> IntV n
  | Bool b -> BoolV b
  | Var id -> Env.apply env id
  | IsZero e -> begin 
    match value_of_expr env e with
    | IntV i -> BoolV (i = 0) 
    | _ -> failwith "IsZero: input has non integer type"
    end
  | Op {op; left; right} -> begin 
    match value_of_expr env left, value_of_expr env right with 
    | IntV i1, IntV i2 -> apply_op op i1 i2
    | _ -> failwith ""
    end
  | If { cond; onTrue; onFalse } -> begin 
    match value_of_expr env cond with
    | BoolV b -> begin match b with 
      | true -> value_of_expr env onTrue
      | false -> value_of_expr env onFalse
      end
    | _ -> failwith "If: condition has non boolean type"
    end
  | Let { var; exp; body } -> 
    let v = value_of_expr env exp in 
    value_of_expr (Env.extend var v env) body
  | LetRec { pname; pvar; pbody; body } -> 
    value_of_expr (Env.extend_rec pname pvar pbody env) body
  | Fun { var; body } -> ClosV {var = var; body = body; env = env}
  | App { rator; rand } -> begin 
    match value_of_expr env rator with
    | ClosV proc -> 
      let v = value_of_expr env rand in 
      value_of_expr (Env.extend proc.var v proc.env) proc.body 
    | _ -> failwith "App: operator has non closure type"
    end

let initial_env = Env.empty()

let value_of_prog = function
| Program expr -> value_of_expr initial_env expr
