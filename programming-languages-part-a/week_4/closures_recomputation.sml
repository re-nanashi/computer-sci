(* Closures and Recomputation *)
fun filter (f, xs) = 
  case xs of
       [] => []
     | x::xs' => if f x 
                 then x::filter(f, xs')
                 else filter(f, xs')

fun allShorterThan1 (xs, s) = 
                  (* Will always recompute s *)
  filter((fn x => String.size(x) < (print "1" ; String.size(s))), xs)

fun allShorterThan2 (xs, s) =
  (* a variable binding is evaluated at its definition, so no recomputation *)
  let val i = (print "1" ; String.size(s))
  in filter((fn x => String.size x < i), xs)
  end
