(* Too few arguments or partial application *)

fun sorted3 x y z = z >= y andalso y >= x 

fun fold f acc xs = 
  case xs of
       [] => acc
     | x::xs' => fold f (f(acc,x)) xs'

(* If a curried function is applied to "too few" arguments,
   that returns, which is often useful.
   A powerful idiom (no new semantics)
*)

val is_nonnegative = sorted3 0 0 

val sum = fold (fn (x,y) => x+y) 0 

(* In fact, not doing this is often a harder-to-notice 
   version of unnecessary function wrapping: 
*)

fun is_nonnegative_inferior x = sorted3 0 0 x

fun sum_inferior xs = fold (fn (x,y) => x + y) 0 xs

(* NOTE: common style is to curry higher-order functions with function 
 * with function arguments
 * *)

(* ormap *)
fun exists predicate xs = 
  case xs of 
       [] => false
      | x::xs' => predicate x orelse exists predicate xs'

val no = exists (fn x => x = 7) [4,11,23] (* false *)

val has_zero = exists (fn x => x = 0)  (* int list -> bool *)

val sample = has_zero [0,4,11,23] (* true *)

(* int list -> int list *)
val increment_all = List.map (fn x => x + 1) 

(* libray functions foldl, List.filter, etc. also curried: *)

val remove_zeros = List.filter (fn x => x <> 0)

(* but if a strange message about "value restriction" pops up,
 * put back in the actually-necessary wrapping or an explicit
 * non-polymorphic type
*)

(* val pair_with_one = List.map (fn x => (x,1)) 
 * ('a list -> ('a * int) list)
 * RETURNS a polymorphic function
*)

(* workarounds: *)
fun pair_with_one xs = List.map (fn x = > (x,1)) xs

val pair_with_one : string list -> (string * int) list =  
  List.map (fn x => (x,1))

(* this funciton works fine because result is not polymorphic *)
val increment_and_pair_with_one = List.map (fn x => (x+1, 1))
