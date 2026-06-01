datatype ExprC =
IdC of string
               | NumC of int
               | StrC of string;

datatype Value =
NumV of int
               | StrV of string
               | BoolV of bool
               | PrimV of string;

(* An environment is a list of ( string, value ) tuples *)
type Env = (string * Value) list 

val top_env : Env = [
  ("true", BoolV true),
  ("false", BoolV false)
                    ]

(* Look something up in an environment *)
fun env_search ((env : Env), (target : string)) : Value = 
  case env of 
       [] => raise Fail ("VEBG4: Environment unable to find: " ^ target)
     | (name, value):: rest => if name = target then value else env_search (rest, target)

(* Invokes the SExp parse function to convert a string into an SExp type *)
fun str_to_sexp (str : string) : SExp.value list =
  SExpParser.parse (TextIO.openString str)

(* Given concrete syntax as a string, parse into an AST node (ExprC)*)
fun parse (concrete : string) : ExprC =
  case (str_to_sexp concrete) of
       [ SExp.INT n ] => NumC (IntInf.toInt n) (* Parsing an int returns an IntInf.toInt and has to be converted to int. https://www.smlnj.org/doc/smlnj-lib/SExp/str-SExp.html *)
     | [ SExp.STRING s ] => StrC s
     | [ SExp.SYMBOL id ] => IdC (Atom.toString id) (* Atoms would make env lookups faster, but just using strings simplifies the code.*)
     | _ => raise Fail ( "VEBG4: bad syntax: " ^ concrete)

(* Given a VEBG4 expression, evaluate it eagerly into its value *)
fun interp (( expr : ExprC ), ( env: Env )) : Value =
  case expr of
  (NumC n) => (NumV n)
  | (StrC s) => (StrV s)
  | (IdC id) => env_search ( env, id )
  
(* serialize a value into a printable string *)
fun serialize (value : Value) : string = 
  case value of
       (StrV str) => str
     | (NumV n) => Int.toString n;

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

(* interp tests *)
val _ = check_equal ("interp: basic int", interp ( (NumC 1), top_env ), NumV 1);
val _ = check_equal ("interp: basic string", interp ( (StrC "hi"), top_env ), StrV "hi");
val _ = check_equal ("interp: true prim", interp ( (IdC "true"), top_env ), BoolV true);
val _ = check_equal ("interp: false prim", interp ( (IdC "false"), top_env ), BoolV false);

(* serialize tests *)
val _ = check_equal_str ("serialize: NumV", serialize (NumV 1), "1");
val _ = check_equal_str ("serialize: StrV", serialize (StrV "hello"), "hello");

(* top interp tests *)
val _ = check_equal_str ("top_interp: basic int", top_interp "3", "3");
val _ = check_equal_str ("top_interp: basic string", top_interp "\"test\"", "test");


val _ = OS.Process.exit OS.Process.success;
