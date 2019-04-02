open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env (st, (s, i, o)) = function
  | []	        	-> (st, (s, i, o))
  | READ	  :: p -> let (z :: r) = i 
				in eval env (z :: st, (s, r ,o)) p
  | WRITE	  :: p -> let (z :: r) = st
				in eval env (r, (s, i, o @ [z])) p
  | BINOP      op :: p -> let (y :: x :: r) = st
				in eval env (Expr.operators op x y :: r, (s, i, o)) p
  | CONST	z :: p -> eval env (z :: st, (s, i, o)) p
  | LD		x :: p -> eval env (s x :: st, (s, i, o)) p
  | ST		x :: p -> let (z :: r) = st
				in eval env (r, (Expr.update x z s, i, o)) p
  | LABEL 	l :: p -> eval env (st, (s, i, o)) p
  | JMP		l :: p -> eval env (st, (s, i, o)) (env#labeled l)
  | CJMP   (x, l) :: p -> let (z :: r) = st
  				in if z == 0 && x = "nz" || z != 0 && x = "z"
  				   then eval env (r, (s, i, o)) p
  				   else eval env (r, (s, i, o)) (env#labeled l) 

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o


(*
let label = ref 0
let genLabel l = 
  let curLabel l = (l:=!l+1; !l) 
  in "L" ^ string_of_int (curLabel l)

Error: This expression has type
         (('a, 'b, 'c, 'd, 'e, 'f) #GT.ref_t -> 'd -> 'b GT.ref -> 'f,
          < fmt : (GT.Format.formatter -> 'b -> GT.unit) ->
                  GT.Format.formatter -> 'b GT.ref -> GT.unit;
            html : ('b -> HTML.er) -> 'b GT.ref -> HTML.er;
            show : ('b -> GT.string) -> 'b GT.ref -> GT.string >)
         GT.t
       This is not a function; it cannot be applied.  
       
DISGUSTING!!1! >_<
*)

let genLabel = object
  val mutable cnt = 0
  method get = (cnt <- cnt + 1; "L" ^ string_of_int cnt)
end

let rec compileL lbl =
  let rec expr = function
    | Expr.Var    x	    	-> [LD x]
    | Expr.Const  n	    	-> [CONST n]
    | Expr.Binop (op, x, y) 	-> expr x @ expr y @ [BINOP op]
  in function
    | Stmt.Seq 	  (s1, s2)  	-> let newlbl = genLabel#get
    				   in let f1, p1 = compileL newlbl s1
    				   in let f2, p2 = compileL lbl s2
    				   in f2, p1 @ (if f1 then [LABEL newlbl] else []) @ p2
    | Stmt.Read    x        	-> false, [READ; ST x]
    | Stmt.Write   e 		-> false, expr e @ [WRITE]
    | Stmt.Assign (x, e)	-> false, expr e @ [ST x]
    | Stmt.Skip		    	-> false, []
    | Stmt.If 	  (e, s1, s2)	-> let loop = genLabel#get
    				   in let f1, p1 = compileL lbl s1
    				   in let f2, p2 = compileL lbl s2
    				   in true, expr e @ [CJMP ("z", loop)] @ p1 
    				   	    @ (if f1 then [] else [JMP lbl]) 
    				   	    @ [LABEL loop] @ p2 @ (if f2 then [] else [JMP lbl])
    | Stmt.While  (e, s1)	-> let cond = genLabel#get
    				   in let loop = genLabel#get
    				   in let l, ps = compileL cond s1	
    				   in false, [JMP cond; LABEL loop] @ ps 
    				   	   @ [LABEL cond] @ expr e @ [CJMP ("nz", loop)]
    | Stmt.Repeat (s1, e)	-> let l1 = genLabel#get
    				   in let l2 = genLabel#get
    				   in let flag, ps = compileL l2 s1
    				   in false, [LABEL l1] @ ps @ [LABEL l2]
    				   	   @ expr e @ [CJMP ("z", l1)]

let rec compile stmt =
  let lbl = genLabel#get 
  in let flag, p = compileL lbl stmt 
  in p @ (if flag then [LABEL lbl] else [])
    
