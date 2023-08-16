open Ast
open Interpreter

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let run input = 
  let ast = parse input in 
  let output = match value_of_prog ast with
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ClosV _ -> "__closure__" in 
  print_newline();
  Printf.printf "%s\n=> %s\n" input output

let _p0 = "
if >(2,1) 
then +(10,1) 
else -(1,10)
"

let _p1 = "
let x = 42 in 
let f = proc (x) -(x,1) in 
(f x)
"

let _p2 = "
let x = 200 in 
let f = proc (z) -(z,x) in 
let x = 100 in 
let g = proc (z) -(z,x) in 
-((f 1), (g 1))
"

let _p3 = "
let x = 1 in 
let y = 2 in 
let f = proc (a) proc (b) +(a,b) in 
((f x) y)
"

let _p4 = "
let makerec = proc (f)
                let d = proc (x) proc (z) ((f (x x)) z)
                in proc (n) ((f (d d)) n)
in let maketimes4 = proc (f)
                      proc (x)
                        if zero?(x)
                        then 0
                        else -((f -(x,1)), -4)
in let times4 = (makerec maketimes4)
in (times4 3)
"

let program = "
letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2)
in (double 6)
"

let _ = run program
