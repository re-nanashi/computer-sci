(* Currying wrapup *)

(* generic functions to switch how/whether currying is used
 * in each case, the type tells you a lot
 * *)

(* returns a function in its curried form*)
fun curry f = fn x => fn y => f(x,y)
fun curry2 f x y = f (x,y)
fun uncurry f (x,y) = f x y

(* example *)
(* tupled but we wish it were curried *)

fun range (i, j) = if i > j then [] else i :: range(i+1, j)
 
(* does not work because range:args is a tuple (yet) *)
(* val countup = range 1 *)

val countup = (curry range) 1
val countup2 = curry2 range 1

val xs = countup 7 (* [1,2,3,4,5,6,7] *)
val xs = countup2 7 (* [1,2,3,4,5,6,7] *)
