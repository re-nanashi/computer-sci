fun swap (pr : int * bool) =
  (#2 pr, #1 pr)

fun sum_two_pairs (pr1: int * int, pr2: int * int) = 
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun sort_pair (pr: int * int) =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr)

val x = (3,(4,(5,6))); 
 (* int * (int * (int * int))*)

val y = (#2 x,(#1 x,#2 (#2 x))); 
(* y = (4,(5,6),(3,(5,6)))*)
(* (int * (int * int)) * (int * (int * int)) *)

val ans = (#2 y,4); 
