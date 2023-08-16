type env =
  | Empty
  | Extend of string * value * env
  | ExtendRec of string * string * expr * env

and closure = { var : string; body : expr; env : env }
and value = IntV of int | BoolV of bool | ClosV of closure

and expr =
  | Int of int
  | Bool of bool
  | Var of string
  | IsZero of expr
  | Op of { op : char; left : expr; right : expr }
  | If of { cond : expr; onTrue : expr; onFalse : expr }
  | Let of { var : string; exp : expr; body : expr }
  | LetRec of { pname : string; pvar : string; pbody : expr; body : expr }
  | Fun of { var : string; body : expr }
  | App of { rator : expr; rand : expr }

type prog = Program of expr
