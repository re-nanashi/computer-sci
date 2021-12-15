(* Idiom: Function Composition: Combining functions *)

(* ('b -> 'a) * ('b -> 'a) -> ('a -> 'c) *)
fun compose(f, g) = fn x => f(g(x))

(* int -> real *)
fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs(i)))

fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs)(i)

(* right -> left*)
fun sqrt_of_abs = Math.sqrt o Real.fromInt o abs

(* |> !>*)
infix !>

fun x !> f = f(x) 

(* Just like linux terminal piping *)
fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt

(* produce the result of g if f is not the right thing *)
fun backup1 (f,g) = 
  fn x => case f(x) of
               NONE => g x
             | SOME y => y

fun backup2 (f,g) = fn x => f(x) handle _ => g(x) 
