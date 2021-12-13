(* Anonymous functions *)

fun n_times (f, n, x) =
  if n = 0
  then x
  else f (n_times(f, n - 1, x))

(* 1st version *)
fun triple x = x * 3
fun triple_n_times_1 (n, x) = n_times(triple, n, x)

(* 2nd version local *)
fun triple_n_times_2 (n, x) = 
  let 
    fun triple x = x * 3
  in 
    n_times(triple, n, x)
  end

(* 3nd version lambda *)
fun triple_n_times_3 (n, x) =
  n_times((fn x => 3 * x), n, x)

(* poor style *)
val triple_n_times_4 = fn (n, x) => n_times((fn x => x * 3), n, x)
