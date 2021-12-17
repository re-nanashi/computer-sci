(* Closure idioms without closures *)

datatype a' mylist = Cons of 'a * ('a mylist) | Empty

fun map f xs = 
  case xs of
       Empty => Empty
     | Cons(x, xs) => Cons(f x, map f xs)

fun filter f x = 
  case xs of
       Empty => Empty
     | Cons(x, xs) => if f x 
                      then Cons(x, filter f x)
                      else filter f xs
fun length xs = 
  case xs of
       Empty => 0
     | Cons(_, xs) => 1 + length xs

(* Double all numbers in a list *)
val doubleAll = map (fn x => x * 2)

(* Count all numbers in a list that is n *)
fun countNs (xs, n : int) = length (filter (fn x => x = n) xs)
