(* Idiom: Currying *)

(* old way to get the effect of multiple arguments *)
fun sorted3_tupled (x, y, z) = z >= y andalso y >= x

val tl = sorted3_tupled(7,9,11)

(* new way: currying *)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t2 = ((sorted3 7) 9) 11
(* Calling (sorted3 7) returns a function with:
    - Code fn(y) => fn(z) => z >= y andalso y >= x || fn1
    - Environment maps x to 7
*)

(* Calling fn1(9) returns a function with:
    - Code fn(z) => z >= y andalso y >= x || fn2
    - Environment maps x to 7, y to 9
*)

(* Calling fn2(11) returns a bool:
    - z >= y andalso y >= x 
    - 11 >= 9 andalso 9 >= 7     || true
    - Environment maps x to 7, y to 9, z to 11
*)

(* Syntactic sugar *)
val t3 = sorted3 7 9 11

(*
val wrong1 = sorted3_tupled 7 9 11
val wrong2 = sorted3(7, 9, 11)
*)

(* Syntactic sugar *)
fun sorted3_nicer x y z = z >= y andalso y >= x

val t4 = sorted3_nicer 7 9 11
val t5 = ((sorted3_nicer 7) 9) 11

(* a more useful example *)
fun fold f acc xs = 
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs'

(* a call to curried fold: will improve this call next *)
