(* Higher Order and lexical scoping *)

(* first example *)
val x = 1

fun f y = 
  let val x = y + 1
  in fn z => x + y + z (* x = y + 1, therefore y + 1 + y + z or 2y + 1 + z *)
  end

val x = 3   (* Irrelevant *)

val g = f 4 (* f z => 9 + z*)

val y = 5   (* Irrelevant *)

val z = g 6 (* 15 *)

(* Second Example *)
fun f g = 
  let val x = 3 (* Irrelevant *)
  in g 2        (* g is a function that calls 2 *) 
  end 

val x = 4
fun h y = x + y (* y => 4 + y ; always add 4 to argument *)
val z = f h     (* 6 *)
