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
(*   case expr of *)
(*        NumC n =>  *)
