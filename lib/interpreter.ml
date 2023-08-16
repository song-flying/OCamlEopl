open Ast
open Environment

let apply_iop op v1 v2 = IntV (op v1 v2)
let apply_bop op v1 v2 = BoolV (op v1 v2)

let apply_op op v1 v2 =
  match op with
  | '+' -> apply_iop ( + ) v1 v2
  | '-' -> apply_iop ( - ) v1 v2
  | '=' -> apply_bop ( = ) v1 v2
  | '>' -> apply_bop ( > ) v1 v2
  | '<' -> apply_bop ( < ) v1 v2
  | _ -> failwith "invalid operator"

module Recursive = struct
  let rec value_of_expr : env -> expr -> value =
   fun env expr ->
    match expr with
    | Int n -> IntV n
    | Bool b -> BoolV b
    | Var id -> Env.apply env id
    | IsZero e -> (
        match value_of_expr env e with
        | IntV i -> BoolV (i = 0)
        | _ -> failwith "IsZero: input has non integer type")
    | Op { op; left; right } -> (
        match (value_of_expr env left, value_of_expr env right) with
        | IntV i1, IntV i2 -> apply_op op i1 i2
        | _ -> failwith "Op: input has non integer type")
    | If { cond; onTrue; onFalse } -> (
        match value_of_expr env cond with
        | BoolV b -> (
            match b with
            | true -> value_of_expr env onTrue
            | false -> value_of_expr env onFalse)
        | _ -> failwith "If: condition has non boolean type")
    | Let { var; exp; body } ->
        let v = value_of_expr env exp in
        value_of_expr (Env.extend var v env) body
    | LetRec { pname; pvar; pbody; body } ->
        value_of_expr (Env.extend_rec pname pvar pbody env) body
    | Fun { var; body } -> ClosV { var; body; env }
    | App { rator; rand } -> (
        match value_of_expr env rator with
        | ClosV proc ->
            let v = value_of_expr env rand in
            value_of_expr (Env.extend proc.var v proc.env) proc.body
        | _ -> failwith "App: operator has non closure type")

  let initial_env = Env.empty ()
  let value_of_prog = function Program expr -> value_of_expr initial_env expr
end

module Iterative = struct
  type cont =
    | EndCont
    | ZeroCont of cont
    | LetCont of { var : string; body : expr; env : Env.t; inner_cont : cont }
    | IfCont of {
        onTrue : expr;
        onFalse : expr;
        env : Env.t;
        inner_cont : cont;
      }
    | Op1Cont of { op : char; right : expr; env : Env.t; inner_cont : cont }
    | Op2Cont of { op : char; left : value; inner_cont : cont }
    | RatorCont of { rand : expr; env : Env.t; inner_cont : cont }
    | RandCont of { proc : closure; inner_cont : cont }

  let rec value_of_expr : env -> expr -> cont -> value =
   fun env expr cont ->
    match expr with
    | Int n -> apply cont (IntV n)
    | Bool b -> apply cont (BoolV b)
    | Var id -> apply cont (Env.apply env id)
    | IsZero e -> value_of_expr env e (ZeroCont cont)
    | Op { op; left; right } ->
        value_of_expr env left (Op1Cont { op; right; env; inner_cont = cont })
    | If { cond; onTrue; onFalse } ->
        value_of_expr env cond
          (IfCont { onTrue; onFalse; env; inner_cont = cont })
    | Let { var; exp; body } ->
        value_of_expr env exp (LetCont { var; body; env; inner_cont = cont })
    | LetRec { pname; pvar; pbody; body } ->
        value_of_expr (Env.extend_rec pname pvar pbody env) body cont
    | Fun { var; body } -> apply cont (ClosV { var; body; env })
    | App { rator; rand } ->
        value_of_expr env rator (RatorCont { rand; env; inner_cont = cont })

  and apply cont value =
    match cont with
    | EndCont -> value
    | ZeroCont inner_cont -> (
        match value with
        | IntV i -> apply inner_cont (BoolV (i = 0))
        | _ -> failwith "IsZero: input has non integer type")
    | LetCont { var; body; env; inner_cont } ->
        value_of_expr (Env.extend var value env) body inner_cont
    | IfCont { onTrue; onFalse; env; inner_cont } -> (
        match value with
        | BoolV b -> (
            match b with
            | true -> value_of_expr env onTrue inner_cont
            | false -> value_of_expr env onFalse inner_cont)
        | _ -> failwith "If: condition has non boolean type")
    | Op1Cont { op; right; env; inner_cont } ->
        value_of_expr env right (Op2Cont { op; left = value; inner_cont })
    | Op2Cont { op; left; inner_cont } -> (
        match (left, value) with
        | IntV i1, IntV i2 -> apply inner_cont (apply_op op i1 i2)
        | _ -> failwith "Op: input has non integer type")
    | RatorCont { rand; env; inner_cont } -> (
        match value with
        | ClosV proc -> value_of_expr env rand (RandCont { proc; inner_cont })
        | _ -> failwith "App: operator has non closure type")
    | RandCont { proc; inner_cont } ->
        value_of_expr (Env.extend proc.var value proc.env) proc.body inner_cont

  let initial_env = Env.empty ()

  let value_of_prog = function
    | Program expr -> value_of_expr initial_env expr EndCont
end

module Imperative = struct
  type cont =
    | EndCont
    | ZeroCont of cont
    | LetCont of { var : string; body : expr; env : Env.t; inner_cont : cont }
    | IfCont of {
        onTrue : expr;
        onFalse : expr;
        env : Env.t;
        inner_cont : cont;
      }
    | Op1Cont of { op : char; right : expr; env : Env.t; inner_cont : cont }
    | Op2Cont of { op : char; left : value; inner_cont : cont }
    | RatorCont of { rand : expr; env : Env.t; inner_cont : cont }
    | RandCont of { proc : closure; inner_cont : cont }

  let expr_reg = ref (Int 0)
  let env_reg = ref (Env.empty ())
  let cont_reg = ref EndCont
  let value_reg = ref (IntV 0)

  let rec value_of_expr () =
    match !expr_reg with
    | Int n ->
        value_reg := IntV n;
        apply ()
    | Bool b ->
        value_reg := BoolV b;
        apply ()
    | Var id ->
        value_reg := Env.apply !env_reg id;
        apply ()
    | IsZero e ->
        cont_reg := ZeroCont !cont_reg;
        expr_reg := e;
        value_of_expr ()
    | Op { op; left; right } ->
        cont_reg :=
          Op1Cont { op; right; env = !env_reg; inner_cont = !cont_reg };
        expr_reg := left;
        value_of_expr ()
    | If { cond; onTrue; onFalse } ->
        cont_reg :=
          IfCont { onTrue; onFalse; env = !env_reg; inner_cont = !cont_reg };
        expr_reg := cond;
        value_of_expr ()
    | Let { var; exp; body } ->
        cont_reg :=
          LetCont { var; body; env = !env_reg; inner_cont = !cont_reg };
        expr_reg := exp;
        value_of_expr ()
    | LetRec { pname; pvar; pbody; body } ->
        expr_reg := body;
        env_reg := Env.extend_rec pname pvar pbody !env_reg;
        value_of_expr ()
    | Fun { var; body } ->
        value_reg := ClosV { var; body; env = !env_reg };
        apply ()
    | App { rator; rand } ->
        cont_reg := RatorCont { rand; env = !env_reg; inner_cont = !cont_reg };
        expr_reg := rator;
        value_of_expr ()

  and apply () =
    match !cont_reg with
    | EndCont -> !value_reg
    | ZeroCont inner_cont -> (
        match !value_reg with
        | IntV i ->
            cont_reg := inner_cont;
            value_reg := BoolV (i = 0);
            apply ()
        | _ -> failwith "IsZero: input has non integer type")
    | LetCont { var; body; env; inner_cont } ->
        cont_reg := inner_cont;
        expr_reg := body;
        env_reg := Env.extend var !value_reg env;
        value_of_expr ()
    | IfCont { onTrue; onFalse; env; inner_cont } -> (
        match !value_reg with
        | BoolV b -> (
            cont_reg := inner_cont;
            env_reg := env;
            match b with
            | true ->
                expr_reg := onTrue;
                value_of_expr ()
            | false ->
                expr_reg := onFalse;
                value_of_expr ())
        | _ -> failwith "If: condition has non boolean type")
    | Op1Cont { op; right; env; inner_cont } ->
        cont_reg := Op2Cont { op; left = !value_reg; inner_cont };
        expr_reg := right;
        env_reg := env;
        value_of_expr ()
    | Op2Cont { op; left; inner_cont } -> (
        match (left, !value_reg) with
        | IntV i1, IntV i2 ->
            cont_reg := inner_cont;
            value_reg := apply_op op i1 i2;
            apply ()
        | _ -> failwith "Op: input has non integer type")
    | RatorCont { rand; env; inner_cont } -> (
        match !value_reg with
        | ClosV proc ->
            cont_reg := RandCont { proc; inner_cont };
            expr_reg := rand;
            env_reg := env;
            value_of_expr ()
        | _ -> failwith "App: operator has non closure type")
    | RandCont { proc; inner_cont } ->
        cont_reg := inner_cont;
        expr_reg := proc.body;
        env_reg := Env.extend proc.var !value_reg proc.env;
        value_of_expr ()

  let initial_env = Env.empty ()

  let value_of_prog = function
    | Program expr ->
        cont_reg := EndCont;
        expr_reg := expr;
        env_reg := initial_env;
        value_of_expr ()
end
