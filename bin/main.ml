open Ast

let value_of_prog = Interpreter.Imperative.value_of_prog

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let run input =
  let ast = parse input in
  let output =
    match value_of_prog ast with
    | IntV i -> string_of_int i
    | BoolV b -> string_of_bool b
    | ClosV _ -> "__closure__"
  in
  print_newline ();
  Printf.printf "%s\n=> %s\n" input output

let _p0 = "\nif >(2,1) \nthen +(10,1) \nelse -(1,10)\n"
let _p1 = "\nlet x = 42 in \nlet f = proc (x) -(x,1) in \n(f x)\n"

let _p2 =
  "\n\
   let x = 200 in \n\
   let f = proc (z) -(z,x) in \n\
   let x = 100 in \n\
   let g = proc (z) -(z,x) in \n\
   -((f 1), (g 1))\n"

let _p3 =
  "\n\
   let x = 1 in \n\
   let y = 2 in \n\
   let f = proc (a) proc (b) +(a,b) in \n\
   ((f x) y)\n"

let _p4 =
  "\n\
   let makerec = proc (f)\n\
  \                let d = proc (x) proc (z) ((f (x x)) z)\n\
  \                in proc (n) ((f (d d)) n)\n\
   in let maketimes4 = proc (f)\n\
  \                      proc (x)\n\
  \                        if zero?(x)\n\
  \                        then 0\n\
  \                        else -((f -(x,1)), -4)\n\
   in let times4 = (makerec maketimes4)\n\
   in (times4 3)\n"

let program =
  "\n\
   letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2)\n\
   in (double 6)\n"

let _ = run program
