(* This is a comment. This is our first program. *)

val x = 34;
(* static enivronment: x : int *)
(* dynamic enivronment: x --> 34 *)

val y = 17;
(* static enivronment: x : int, y : int *)
(* dynamic enivronment: x --> 34, y--> 17 *)

val z = (x + y) + (y + 2);
(* static enivronment: x : int, y : int, z : int *)
(* dynamic enivronment: x --> 34, y--> 17, z --> 70 *)

val q = z + 1
(* static enivronment: x : int, y : int, z : int, q : int *)
(* dynamic enivronment: x --> 34, y--> 17, z --> 70, q --> 71 *)

val abs_of_z = if z < 0 
               then 0 - z 
               else z; (* bool *) (* int *)
(* abs_of_z : int *)
(* dynamic enivronment : ..., abs_of_z --> 70 *)
 
val abs_of_z_simpler = abs z;

fun pow_helper(x : int, y : int, acc : int) =
  if y = 0 
  then acc
  else pow_helper(x, (y - 1), (x * acc))

fun pow(x: int, y: int) = 
  pow_helper(x, y, 1)

val test1 = pow(3,2) = 9

fun cube(x : int) = 
  pow(x, 3)

