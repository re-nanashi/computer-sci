val x = 42 (* val x : int *)

fun f (y,z,w) =
  if y          (* y must be a bool *)
  then z + x    (* z must be an int *)
  else 0        (* both branches have same type : int *)
(* f must return an int 
 * f must take a bool * int * anything 
 * so val f : bool * int * 'a -> int *)
