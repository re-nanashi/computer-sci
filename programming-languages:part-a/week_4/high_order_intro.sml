(* Introduction to First-class functions *)

fun double x = 2 * x
fun incr x = x + 1
val a_tuple = (double, incr, double(incr 7))
(* val eighteen calls the #1 function in a_tuple *)
val eighteen = (#1 a_tuple) 9
