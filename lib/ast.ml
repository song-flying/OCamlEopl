type env =
  | Empty
  | Extend of string * value * env
  | ExtendRec of string * string * expr * env

and closure = { var : string; body : expr; env : env }

and cont =
  | EndCont
  | ZeroCont of cont
  | LetCont of { var : string; body : expr; env : env; inner_cont : cont }
  | IfCont of { onTrue : expr; onFalse : expr; env : env; inner_cont : cont }
  | Op1Cont of { op : char; right : expr; env : env; inner_cont : cont }
  | Op2Cont of { op : char; left : value; inner_cont : cont }
  | RatorCont of { rand : expr; env : env; inner_cont : cont }
  | RandCont of { proc : closure; inner_cont : cont }
  | ThrowCont of { rand : expr; env : env }

and value = IntV of int | BoolV of bool | ClosV of closure | ContV of cont

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
  | LetCC of { var : string; body : expr }
  | Throw of { rand : expr; rator : expr }

type prog = Program of expr
