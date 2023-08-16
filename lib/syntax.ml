type lambdaExp =
  | Var of string
  | Lam of string * lambdaExp
  | App of lambdaExp * lambdaExp

let rec occurs_free var = function
  | Var id -> id = var
  | Lam (id, expr) -> if id = var then false else occurs_free var expr
  | App (expr1, expr2) -> occurs_free var expr1 || occurs_free var expr2
;;

assert (occurs_free "x" (Lam ("y", App (Lam ("x", Var "y"), Var "x"))))

type sexp = Atom of string | List of sexp list

let rec subst new_sym old_sym = function
  | Atom sym -> Atom (if sym = old_sym then new_sym else sym)
  | List sexps -> List (List.map (subst new_sym old_sym) sexps)
;;

assert (
  subst "x" "a"
    (List
       [
         Atom "a";
         List [ Atom "b"; Atom "a"; List [ Atom "a" ]; Atom "c" ];
         Atom "a";
       ])
  = List
      [
        Atom "x";
        List [ Atom "b"; Atom "x"; List [ Atom "x" ]; Atom "c" ];
        Atom "x";
      ])
