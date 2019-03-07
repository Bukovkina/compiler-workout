(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    
    
	(* val boolToInt : bool -> int *)
	let boolToInt expr = if expr then 1 else 0

	(* val intToBool : int -> bool *)
	let intToBool expr = expr <> 0

	(* val operators : string -> int -> int -> int *)
	let operators = function
		| "+" 	-> ( + )
		| "-" 	-> ( - )
		| "*" 	-> ( * )
		| "/" 	-> ( / )
		| "%" 	-> ( mod )
		| "<" 	-> fun a b -> boolToInt (a <  b)
		| "<="	-> fun a b -> boolToInt (a <= b)
		| ">"  	-> fun a b -> boolToInt (a >  b)
		| ">=" 	-> fun a b -> boolToInt (a >= b)
		| "==" 	-> fun a b -> boolToInt (a == b)
		| "!=" 	-> fun a b -> boolToInt (a <> b)
		| "&&" 	-> fun a b -> boolToInt ((intToBool a) && (intToBool b))
		| "!!" 	-> fun a b -> boolToInt ((intToBool a) || (intToBool b)) 
		| _ 	-> failwith ("Unknown Operator :c")

	let rec eval st expr = match expr with
		| Const  n 			 -> n
		| Var 	 x 			 -> st x 
		| Binop (oper, a, b) -> operators oper (eval st a) (eval st b)


	let binopParser oper = ostap (- $(oper)), (fun x y -> Binop (oper, x, y))
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)
    ostap (
    	expr:
    		!(Ostap.Util.expr
    			(fun x -> x)
    			(Array.map (fun (asc, op) -> asc, List.map binopParser op) 
					[|
						'Lefta, ["!!"];
						'Lefta, ["&&"];
						'Nona,	["<="; "<"; ">="; ">"; "=="; "!="];
						'Lefta, ["+"; "-"];
						'Lefta, ["*"; "/"; "%"];
					|]
    			)
    			primary
    		);
    		
    	primary: x:IDENT {Var x} | n:DECIMAL {Const n} | -"(" expr -")"
	)    

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
	let rec eval (s, i, o) state = match state with
	   	| Read		 x	-> (Expr.update x (List.hd i) s, List.tl i, o)
	   	| Write		 e	-> (s, i, o @ [Expr.eval s e])
	  	| Assign	(x, e)	-> (Expr.update x (Expr.eval s e) s, i, o)
		| Seq	(s1, s2)-> eval (eval (s, i, o) s1) s2

    (* Statement parser *)
    ostap (
      stmt: 
      	  x:IDENT 	":=" 	e:!(Expr.expr)		{Assign (x, e)}
      	| "read" 	"("		x:IDENT			")"	{Read x}
      	| "write"	"("		e:!(Expr.expr)	")"	{Write e};
      
      parse: 
      	  s1:stmt 	";" 	s2:parse 			{Seq (s1, s2)}
      	| stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
