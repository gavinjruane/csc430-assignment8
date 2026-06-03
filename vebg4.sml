datatype ExprC = IdC of string
               | NumC of int
               | StrC of string
               | IfC of ExprC * ExprC * ExprC
               | LamC of string list * ExprC (* (params, body) *)
               | AppC of ExprC * ExprC list (* (expr, args) *)

datatype Value = NumV of int
               | StrV of string
               | BoolV of bool
               | PrimV of string
               | CloV of string list * ExprC * (string * Value) list (* (params, body, env) *)
               (* need to figure out recursive types for using Env *)

(* --------- ENVIRONMENT --------- *)
(* An environment is a list of ( string, value ) tuples *)
type Env = (string * Value) list

(* Top level environment *)
val top_env : Env = [
  ("true", BoolV true),
  ("false", BoolV false),
  ("strlen", PrimV "strlen"),
  ("substring", PrimV "substring"),
  ("<=", PrimV "<="),
  ("equal?", PrimV "equal?")
]

(* --------- HELPERS --------- *)
(* Generic function for searching over a list. *)
(* The quote syntax is syntax for generics. The double quote means the generic
* type supports equality comparison.*)
fun list_search (( elem_list : (''a * 'b) list ), ( target : ''a ), err_msg : string): 'b = 
  case elem_list of
       [] => raise Fail err_msg
     | (key, value)::rest => if key = target then value else list_search (rest, target, err_msg)

(* Invokes the SExp parse function to convert a string into an SExp type *)
fun str_to_sexp (str : string) : SExp.value list =
  SExpParser.parse (TextIO.openString str)

fun sub_args ((params : string list), (args : Value list), (env : Env)): Env =
  ListPair.foldl (fn ((param : string), (arg : Value), (env : Env)) =>
                    (param, arg) :: env)
                  env
                  (params, args)

(* Look something up in an environment *)
fun env_search ((env : Env), (target : string)) : Value = 
  list_search (env, target, "VEBG4: Unable to find identifier in environment")

(* --------- PRIMITIVES --------- *)

fun prim_strlen (vals : Value list) : Value =
  case vals of
       [ StrV s ] => NumV (String.size s)
     | _ => raise Fail ("VEBG4: invalid arguments passed to strlen primitive")


(* Get the substring of a string given a desired start index (inclusive) and stop index (exclusive) *)
(* NOTE: substring in SML is a bit weird; it takes a string, start index, and desired length, not a stop.*)
fun prim_substring (vals : Value list) : Value =
case vals of [StrV str, NumV start, NumV stop] => if (start > stop)
                                   then raise Fail ("VEBG4: stop index is before start index")
                                   else if (start >= (String.size str))
                                   then raise Fail ("VEBG4: start index out of range")
                                   else if (stop > (String.size str))
                                   then raise Fail ("VEBG4: stop index out of range")
                                   else StrV (substring (str, start, stop))
           | _ => raise Fail ("VEBG4: invalid arguments passed to substring primitive");

(* Return whether A is less than or equal to B (or error if either are not numbers)*)
fun prim_leq (vals : Value list) : Value =
case vals of [NumV a, NumV b] => BoolV (a <= b)
           | _ => raise Fail ("VEBG4: must provide two arguments of type num");

(* Return whether the two values are equal, provided that they are not primitives or closures *)
fun prim_equal (vals : Value list) : Value =
case vals of [StrV s1, StrV s2] => BoolV (s1 = s2)
           | [NumV n1, NumV n2] => BoolV (n1 = n2)
           | [BoolV b1, BoolV b2] => BoolV (b1 = b2)
           | [_, _] => BoolV false
           | _ => raise Fail "VEBG4: arity mismatch to equal?, must provide 2 arguments"

(* Map a primitive to a function*)
val prim_tbl : (string * (Value list -> Value)) list = [
  ("strlen", prim_strlen),
  ("substring", prim_substring),
  ("<=", prim_leq),
  ("equal?", prim_equal)
]

fun prim_search (target : string) : Value list -> Value =
  list_search (prim_tbl, target, "VEBG4: Unable to find primitive in primitive table")

(* Given concrete syntax as a string, parse into an AST node (ExprC)*)
fun parse (concrete : string) : ExprC =
  case (str_to_sexp concrete) of
       [ SExp.INT n ] => NumC (IntInf.toInt n) (* Parsing an int returns an IntInf.toInt and has to be converted to int. https://www.smlnj.org/doc/smlnj-lib/SExp/str-SExp.html *)
     | [ SExp.STRING s ] => StrC s
     | [ SExp.SYMBOL id ] => IdC (Atom.toString id) (* Atoms would make env lookups faster, but just using strings simplifies the code.*)
     (* | [ SExp.LIST [ SExp.SYMBOL "if", SExp.value cond, SExp.value iftrue, SExp.value iffalse ] ] => IfC (parse cond, parse iftrue, parse iffalse) *)
     (* | SExp.LIST [ SExp.SYMBOL (Atom.atom "fn"), SExp.LIST params, SExp.SYMBOL (Atom.atom "->"), body ] =>
          LamC (["x"], StrC "lol") *)
     | _ => raise Fail ( "VEBG4: bad syntax: " ^ concrete)

(* Given a VEBG4 expression, evaluate it eagerly into its value *)
fun interp (( expr : ExprC ), ( env: Env )) : Value =
  case expr of
      (NumC n) => (NumV n)
    | (StrC s) => (StrV s)
    | (IdC id) => env_search ( env, id )
    | (IfC (cond, iftrue, iffalse)) =>
        (case interp (cond, env) of (BoolV true) => interp (iftrue, env)
                                  | (BoolV false) => interp (iffalse, env)
                                  | _ => raise Fail "VEBG4: condition did not return a bool")
    | (LamC (params, body)) => CloV (params, body, env)
    | (AppC (fn_expr, args)) => 
        (case interp (fn_expr, env) of
            (CloV (params, body, clo_env)) =>
                if length params = length args
                then interp (body,
                             sub_args (params,
                                       map (fn ((arg : ExprC)) => interp (arg, env)) args,
                                       clo_env))
                else raise Fail "VEBG4: arity mismatch"
          | (PrimV name) => prim_search name (map (fn ((arg : ExprC)) => interp (arg, env)) args)
          | _ => raise Fail "VEBG4: invalid function application")
  
  
(* serialize a value into a printable string *)
fun serialize (value : Value) : string = 
  case value of
       (StrV str) => str
     | (NumV n) => Int.toString n
     | (BoolV b) => Bool.toString b
     | (CloV _) => "#<procedure>"
     | (PrimV _) => "#<primop>"

fun top_interp (vebg4 : string) : string =
  serialize (interp ( (parse vebg4), top_env ))

(* --------- TESTING HELPERS --------- *)

(* Recreating racket testing functions *)
(* Basic test case . Checks if an actual matches expected and prints pass/fail depending on result. *)
(* Takes a tuple  *)
fun check_equal ( name, ( actual : Value ), ( expected : Value) ) : unit = 
  (print 
  ("check_equal: " ^ name ^ ": " ^ 
  (if expected = actual
   then "\027[32mPASS\027[0m\n" 
   else "\027[31mFAIL\027[0m\n")));

(* check if two strings are equal *)
fun check_equal_str ( name, ( actual : string ), ( expected : string ) ) : unit = 
  (print 
  ("check_equal_str: " ^ name ^ ": " ^ 
  (if expected = actual
   then "\027[32mPASS\027[0m\n" 
   else "\027[31mFAIL\027[0m\n")));

(* check if two expressions are equal. used for parse tests *)
fun check_equal_expr ( name, ( actual : ExprC ), ( expected : ExprC ) ) : unit = 
  (print 
  ("check_equal_expr: " ^ name ^ ": " ^ 
  (if expected = actual
   then "\027[32mPASS\027[0m\n" 
   else "\027[31mFAIL\027[0m\n")));


(* --------- TESTING --------- *)
(* NOTE: 'val _ = ' is so we can ignore the return value of check_equal.
* Otherwise it gets printed when program is run. *)

(* parse tests *)
val _ = check_equal_expr ("parse: basic int", parse "3", NumC 3);
val _ = check_equal_expr ("parse: basic string", parse "\"hi\"", StrC "hi");
val _ = check_equal_expr ("parse: basic id", parse "+", IdC "+");
val _ = check_equal_expr ("parse: true bool", parse "true", IdC "true");
val _ = check_equal_expr ("parse: false bool", parse "false", IdC "false");
(* val _ = check_equal_expr ("parse: if", parse "(if true 100 200)" IfC ((IdC "true"), (NumC 100), (NumC 200))); *)
(* val _ = check_equal_expr ("parse: lambda fun",
                          parse "fn (x) -> x",
                          LamC (["x"], IdC "x")) *)

(* interp tests *)
val _ = check_equal ("interp: basic int", interp ( (NumC 1), top_env ), NumV 1);
val _ = check_equal ("interp: basic string", interp ( (StrC "hi"), top_env ), StrV "hi");
val _ = check_equal ("interp: true prim", interp ( (IdC "true"), top_env ), BoolV true);
val _ = check_equal ("interp: false prim", interp ( (IdC "false"), top_env ), BoolV false);
val _ = check_equal ("interp: lambda",
                    interp (LamC (["x", "y"], (StrC "this is the body")), top_env),
                    (CloV (["x", "y"], (StrC "this is the body"), top_env)))
val _ = check_equal ("interp: app lambda with argument",
                    interp (AppC (LamC (["x"], (IdC "x")), [(NumC 10)]), top_env),
                    (NumV 10))
val _ = check_equal ("interp: app strlen",
                    interp (AppC ((IdC "strlen"), [(StrC "my string")]), top_env),
                    (NumV 9))
val _ = check_equal ("interp: app <=",
                    interp (AppC ((IdC "<="), [(NumC 1), (NumC 3)]), top_env),
                    (BoolV true))
val _ = check_equal ("interp: basic if",
                     interp ( (IfC ((IdC "true"), (NumC 100), (NumC 200))), top_env ),
                     (NumV 100));
val _ = check_equal ("interp: basic if 2",
                     interp ( (IfC ((IdC "false"), (NumC 100), (NumC 200))), top_env ),
                     (NumV 200));

(* serialize tests *)
val _ = check_equal_str ("serialize: NumV", serialize (NumV 1), "1");
val _ = check_equal_str ("serialize: StrV", serialize (StrV "hello"), "hello");
val _ = check_equal_str ("serialize: BoolV true", serialize (BoolV true), "true");
val _ = check_equal_str ("serialize: BoolV false", serialize (BoolV false), "false");
val _ = check_equal_str ("serialize: closure", serialize (CloV (["x", "y"], (StrC "Closure"), top_env)), "#<procedure>")
val _ = check_equal_str ("serialize: primitive", serialize (PrimV "strlen"), "#<primop>")

(* top interp tests *)
val _ = check_equal_str ("top_interp: basic int", top_interp "3", "3");
val _ = check_equal_str ("top_interp: basic string", top_interp "\"test\"", "test");
val _ = check_equal_str ("top_interp: basic id lookup", top_interp "true", "true");

(* strlen tests *)
val _ = check_equal ("strlen: basic string", prim_strlen [(StrV "Hello!")], (NumV 6));

(* substring tests *)
val _ = check_equal ("substring: basic arguments", prim_substring [(StrV "hey!"), (NumV 2), (NumV 2)], (StrV "y!"));
val _ = check_equal ("substring: simple test", prim_substring [(StrV "Hello!"), (NumV 1), (NumV 3)], (StrV "ell"));

(* leq tests *)
val _ = check_equal ("<=: basic arguments", prim_leq [(NumV 3), (NumV 5)], (BoolV true));
val _ = check_equal ("<=: basic arguments 2", prim_leq [(NumV 5), (NumV 3)], (BoolV false));
(* val _ = check_equal ("<=: negative arguments", prim_leq [(NumV -10), (NumV 10)], (BoolV true)); *)

(* equal? tests *)
val _ = check_equal ("equal?: true = true", prim_equal [(BoolV true), (BoolV true)], (BoolV true));
val _ = check_equal ("equal?: true = false", prim_equal [(BoolV true), (BoolV false)], (BoolV false));
val _ = check_equal ("equal?: 300 = 300", prim_equal [(NumV 300), (NumV 300)], (BoolV true));
val _ = check_equal ("equal?: 3 = 300", prim_equal [(NumV 3), (NumV 300)], (BoolV false));
val _ = check_equal ("equal?: 'hello!' = 'hello!'", prim_equal [(StrV "hello!"), (StrV "hello!")], (BoolV true));
val _ = check_equal ("equal?: 'hello!' = 'goodbye!'", prim_equal [(StrV "hello!"), (StrV "goodbye!")], (BoolV false));
val _ = check_equal ("equal?: 'hello!' = <primitive>", prim_equal [(StrV "hello!"), (PrimV "equal?")], (BoolV false));


val _ = OS.Process.exit OS.Process.success;
