(* Being able to pass closures that have free variables 
 * makes h-o functions much more useful *)

fun filter (f, xs) = 
  case xs of
       [] => []
     | x::xs' => if f x 
                 then x::filter(f, xs')
                 else filter(f, xs')

fun greaterThanX x = fn y => y > x

fun noNegatives xs = filter(greaterThanX(~1), xs)  

fun allGreater (xs, n) = filter(greaterThanX(n), xs)

fun allGreater_1 (xs, n) = filter((fn x => x > n), xs)
