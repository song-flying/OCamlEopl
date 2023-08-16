open Ast

module Env = struct
  type env = Ast.env

  let empty () = Empty
  let extend id value env = Extend (id, value, env)
  let extend_rec pname pvar pbody env = ExtendRec (pname, pvar, pbody, env)

  let rec apply env id =
    match env with
    | Empty -> failwith "no binding found"
    | Extend (id', value, env') -> if id = id' then value else apply env' id
    | ExtendRec (pname, pvar, pbody, env') ->
        if id = pname then ClosV { var = pvar; body = pbody; env }
        else apply env' id
end
