(* Fold and More Closures *)

fun fold (f, acc, xs) = 
  case xs of
       [] => acc
     | (x::xs') => fold (f, f(acc, x), xs')

(* NOTE this is "fold left" if order 
   matters can also do "fold right"*)

(* examples not using private data *)

(* Sum *)
fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)

(* All positive *)
fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* Counting the elements that is in range *)
fun f3 (xs, lo, hi) = 
  fold ((fn (x, y) => 
          x + (if y >= lo andalso y <= hi 
               then 1 
               else 0)), 
        0, xs)

(* Produces true if all elements is less shorter than s *)
fun f4 (xs, s) = 
  let val i = String.size s
  in fold((fn (x, y) => x andalso String.size y < i), true, xs)
  end

(* Abstract andmap *)
fun f5 (g, xs) = fold((fn (x,y) => x andalso g y), true, xs)
(* Abstract ormap *)
fun ormap (g, xs) = fold((fn (x,y) => x orelse g y), false, xs)

fun f4again(xs, s) = 
  let val i = String.size s
  in f5((fn y => String.size y < i), xs)
  end
