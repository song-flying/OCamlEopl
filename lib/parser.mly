%{
open Ast
%}

%token <int>    INT
%token <string> ID
%token DIFF
%token PLUS
%token GREATER
%token LESS
%token COMMA
%token LPAREN
%token RPAREN
%token LET
%token LETREC
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token ZERO
%token PROC
%token LETCC
%token THROW
%token TO
%token EOF

%start <Ast.prog> prog

%%

prog:
	| e = expr;     EOF  { Program e }
	;

expr:
	| x = ID  { Var x }
	| i = INT { Int i }
	| ZERO;    LPAREN; e = expr; RPAREN                     { IsZero e }
	| DIFF;    LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN  { Op { op = '-'; left = e1; right = e2 } }
	| PLUS;    LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN  { Op { op = '+'; left = e1; right = e2 } }
	| EQUALS;  LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN  { Op { op = '='; left = e1; right = e2 } }
	| GREATER; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN  { Op { op = '>'; left = e1; right = e2 } }
	| LESS;    LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN  { Op { op = '<'; left = e1; right = e2 } }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr       { If  { cond = e1; onTrue = e2; onFalse = e3 } }
	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr         { Let { var = x; exp = e1; body = e2 } }
	| LETREC; f = ID; LPAREN; x = ID; RPAREN; EQUALS; e1 = expr; IN; e2 = expr { LetRec { pname = f; pvar = x; pbody = e1; body = e2 } }
	| PROC; LPAREN; x = ID; RPAREN; e = expr                { Fun { var = x; body = e } }
	| LPAREN; e1 = expr; e2 = expr; RPAREN                  { App { rator = e1; rand = e2 } }
	| LETCC; x = ID; IN; e = expr                           { LetCC { var = x; body = e } }
	| THROW; e1 = expr; TO; e2 = expr                       { Throw { rand = e1; rator = e2 } }
	| LPAREN; e = expr; RPAREN { e }
	;
