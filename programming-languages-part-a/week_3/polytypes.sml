(* Section 2: Polymorphic Types and Equality types *)

(* 'a list * 'a list -> 'a list *)
fun append (xs, ys) = 
  case xs of 
       [] => ys
     | x::xs' => x :: append(xs', ys)

val ok1 = append(["hi", "bye"], ["programming", "languages"])

val ok2 = append([1,2], [4,5]);

(* 
val not_ok = append([1,2], ["programming", "languages"])


The type 'a list * 'a list -> 'a list << MORE GENERAL THAN
the type string list * string list -> string list
 
The general type could be used to any less general type like
the type int list * int list -> int list

But it is not more general than the type
int list * string list -> int list
*)

type foo = int * int
type type1 = {quux : 'b, bar : int * 'a, baz: 'b}
(* type1 is more general than *)
type type2 = {quux : string, bar : foo, baz : string}
(* equivalent *)
type type3 = {bar : int * int, baz : string, quux : string}

(* ONLY ON ML*)
(* Equality types *)
(* ''a * ''a -> string *)
fun same_thing (x, y) = 
  if x=y then "yes" else "no"

(* int -> string *)
fun is_three x = 
  if x=3 then "yes" else "no"
