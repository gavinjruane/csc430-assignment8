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


(* fun interp =  *)
  (* case expr of *)
  (*      NumC n =>  *)

(* Recreating racket testing functions *)
(* Basic test case . Checks if an actual matches expected and prints pass/fail depending on result. *)
fun check_equal ( name, actual, expected ) = 
  (print 
  ("check-equal?: " ^ name ^ ": " ^ 
  (if expected = actual then "\027[32mPASS\027[0m\n" else "\027[31mFAIL\027[0m\n")));

check_equal ( "test", 1, 2 );
check_equal ( "test2", 1, 1 );

OS.Process.exit OS.Process.success;
