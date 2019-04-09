(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators

(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let push_scope st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let drop_scope st st' = {st' with g = st.g}

  end
    
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
      
    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let itb b = b <> 0 
    
    let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval st expr =      
      match expr with
      | Const n -> n
      | Var   x -> State.eval st x
      | Binop (op, x, y) -> to_func op (eval st x) (eval st y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
      expr:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var x}
      | -"(" expr -")"
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
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = State.t * int list * int list 

    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment supplies the following method

           method definition : string -> (string list, string list, t)

       which returns a list of formal parameters and a body for given definition
    *)
    let rec eval env (s, i, o) = function
	| Read	 	  x	-> (State.update x (List.hd i) s, List.tl i, o)
	| Write	 	  e	-> (s, i, o @ [Expr.eval s e])
	| Assign      (x, e)	-> (State.update x (Expr.eval s e) s, i, o)
	| Seq	    (s1, s2)  	-> eval env (eval env (s, i, o) s1) s2
	| Skip			-> (s, i, o)
	| If     (e, s1, s2)	-> eval env (s, i, o) (if (Expr.eval s e) != 0 then s1 else s2)  
	| While      (e, s1)	-> if (Expr.eval s e) != 0 then eval env (eval env (s, i, o) s1) (While (e, s1)) else (s, i, o)
	| Repeat     (s1, e)	-> let (s', i', o') = eval env (s, i, o) s1
	  			   in if (Expr.eval s' e) == 0 then eval env (s', i', o') (Repeat (s1, e)) else (s', i', o') 
	| Call	      (f, a)	-> let (params, locals, body) = env#definition f
				   in let fstate = State.push_scope s (params @ locals)
				   in let args = List.map (Expr.eval s) a
				   in let updater state argname argvalue = State.update argname argvalue state
				   in let fscope = List.fold_left2 updater fstate params args
				   in let (s', i', o') = eval env (fscope, i, o) body
				   in (State.drop_scope s' s, i', o')
     
                                
    (* Statement parser *)
    ostap (
       stmt: 
      	    x: IDENT 	":="	e: !(Expr.expr)		{Assign (x, e)}
      	  | "read"	"(" 	x: IDENT	  ")"	{Read x}
      	  | "write"	"(" 	e: !(Expr.expr)	  ")"	{Write e}
      	  | "skip"					{Skip}
      	  | "if"		e: !(Expr.expr)
      	    "then"		s1: parse	
      	    			s2: elses 		{If (e, s1, s2)}
      	  | "while"		e: !(Expr.expr)
      	    "do"		s1: parse
      	    "od"					{While (e, s1)}
      	  | "repeat"		s1: parse
      	    "until"		e: !(Expr.expr)		{Repeat (s1, e)}
      	  | "for"		s1: parse	   ","
      	  			e: !(Expr.expr)	   ","
      	  			s2: parse	
      	    "do"		s3: parse
      	    "od"					{Seq (s1, While (e, Seq (s3, s2)))}
      	  | f: IDENT	"("	a: (!(Expr.expr))* ")"	{Call (f, a)} ;
      	    
      elses:
      	    "fi"					{Skip}
      	  | "else"		s: parse	  "fi"	{s}
      	  | "elif"		e: !(Expr.expr)
      	    "then"		s1: parse
      	    			s2: elses		{If (e, s1, s2)};
      
      parse: 
      	    s1: stmt 	";" 	s2: parse 		{Seq (s1, s2)}
      	  | stmt
	)     
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

    ostap (                                      
      parse:
        "fun"	f: IDENT	
        "(" 	args: (IDENT)*   	")"
        	locals: (%"local" (IDENT)*)?
        "{"	body: !(Stmt.parse)  	"}"
        {
          f, (args, (
              match locals with
                None   -> [] 
              | Some l -> l
           ), body)
        }
    ) 
    

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m        = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o  = Stmt.eval (object method definition f = snd @@ M.find f m end) (State.empty, i, []) body in o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
