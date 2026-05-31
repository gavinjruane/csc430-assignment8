datatype ExprC =
IdC of string
               | NumC of int
               | StrC of string;

datatype Value =
NumV of int
               | StrV of string;

(* Invokes the SExp parse function to convert a string into an SExp type *)
fun str_to_sexp (str : string) : SExp.value list =
  SExpParser.parse (TextIO.openString str)

(* Given concrete syntax as a string, parse into an AST node (ExprC)*)
fun parse (concrete : string) : ExprC =
  case (str_to_sexp concrete) of
       [ SExp.INT n ] => NumC (IntInf.toInt n) (* Parsing an int returns an IntInf.toInt and has to be converted to int. https://www.smlnj.org/doc/smlnj-lib/SExp/str-SExp.html *)
     | [ SExp.STRING s ] => StrC s
     | _ => raise Fail ( "VEBG4: bad syntax: " ^ concrete)

(* Given a VEBG4 expression, evaluate it eagerly into its value *)
fun interp (expr : ExprC) : Value =
  case expr of
  (NumC n) => (NumV n)
  | (StrC s) => (StrV s)
  | _ => raise Fail ( "VEBG4: Unhandled Expression in interp." ); 
  

(* Recreating racket testing functions *)
(* Basic test case . Checks if an actual matches expected and prints pass/fail depending on result. *)
(* Takes a tuple  *)
fun check_equal ( name, ( actual : Value ), ( expected : Value) ) : unit = 
  (print 
  ("check_equal: " ^ name ^ ": " ^ 
  (if expected = actual
   then "\027[32mPASS\027[0m\n" 
   else "\027[31mFAIL\027[0m\n")));

(* interp tests *)
(* NOTE: 'val _ = ' is so we can ignore the return value of check_equal. *)
val _ = check_equal ("interp: basic int", interp (NumC 1), NumV 1);
val _ = check_equal ("interp: basic string", interp (StrC "hi"), StrV "hi");

(* parse tests *)
(* wrapped it in interp so that i don't have to write another check_equal for
* ExprC. If interp is breaking, these will break too.*)
val _ = check_equal ("parse: basic int", interp (parse "3"), NumV 3);
val _ = check_equal ("parse: basic string", interp (parse "hi"), NumV 3);

val _ = OS.Process.exit OS.Process.success;
