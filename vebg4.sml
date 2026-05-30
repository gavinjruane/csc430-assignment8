(* int *)
(* real *)
(* bool *)
(* string *)
(* char *)
(* unit *)
(* Examples: *)
(* 42          (* int *) *)
(* 3.14        (* real *) *)
(* true        (* bool *) *)
(* "hello"     (* string *) *)
(* #"a"        (* char *) *)
(* ()          (* unit *) *)

datatype ExprC =
IdC of string
               | NumC of int
               | StrC of string

datatype Value =
NumV of int
               | StrV of string


(* Given a VEBG4 expression, evaluate it eagerly into its value *)
fun interp (expr : ExprC) : Value =
  case expr of
  (NumC n) => (NumV n)
  | (StrC s) => (StrV s)
  | _ => raise Fail ( "Unhandled Expression in interp." ); 
  

(* Recreating racket testing functions *)
(* Basic test case . Checks if an actual matches expected and prints pass/fail depending on result. *)
(* Takes a tuple  *)
fun check_equal ( name, ( actual : Value ), ( expected : Value) ) : unit = 
  (print 
  ("check_equal: " ^ name ^ ": " ^ 
  (if expected = actual then "\027[32mPASS\027[0m\n" else "\027[31mFAIL\027[0m\n")));

(* interp tests *)
check_equal ("basic int", (interp (NumC 1)), (NumV 1));
check_equal ("basic string", (interp (StrC "hi")), (StrV "hi"));

OS.Process.exit OS.Process.success;
