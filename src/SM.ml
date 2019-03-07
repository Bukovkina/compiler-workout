open GT       
open Language
open Syntax
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval (st, (s, i, o)) pr = match pr with
	| []		     -> (st, (s, i, o))
	| READ		:: p -> let (z :: r) = i in
						eval (z :: st, (s, r ,o)) p
	| WRITE		:: p -> let (z :: r) = st in
						eval (r, (s, i, o @ [z])) p
	| BINOP	op	:: p -> let (y :: x :: r) = st in
						eval (Expr.operators op x y :: r, (s, i, o)) p
	| CONST	z	:: p -> eval (z :: st, (s, i, o)) p
	| LD	x	:: p -> eval (s x :: st, (s, i, o)) p
	| ST	x	:: p -> let (z :: r) = st in
						eval (r, (Expr.update x z s, i, o)) p

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
 
let rec compileExpr e = match e with
	| Expr.Const	 n			-> [CONST n]
	| Expr.Var	 	 x			-> [LD x]
	| Expr.Binop	(op, a, b)	-> (compileExpr a) @ (compileExpr b) @ [BINOP op]

let rec compile t = match t with
	| Stmt.Read	 	 x			-> [READ; ST x]
	| Stmt.Write	 e			-> (compileExpr e) @ [WRITE]
	| Stmt.Assign	(x, e)		-> (compileExpr e) @ [ST x]
	| Stmt.Seq		(s1, s2)	-> (compile s1) @ (compile s2)
