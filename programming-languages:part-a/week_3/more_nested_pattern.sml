(* Section 2: More Nested Pattern *)

(* int list -> bool *)
(* don't do *)
fun nondecreasing xs = 
  case xs of 
       [] => true
     | x::xs' => case xs' of
                      [] => true
                    | y::ys' => x <= y andalso nondecreasing xs'

fun nondecreasing xs =
  case xs of
       [] => true
     | x::[] => true
    (* _::[] => true *)
     | head::(neck::rest) => head <= neck andalso nondecreasing(neck::rest)

datatype sgn = P | N | Z

(* int * int -> sgn *)
fun multsign (x1, x2) = 
  let fun sign x = if x=0 then Z else if x>0 then P else N
  in 
    case (sign x1, sign x2) of
         (Z,_) => Z
       | (_,Z) => Z
       | (P,P) => P
       | (N,N) => P
       | _ => N
(*     | (N,P) => N
       | (P,N) => N
*)
  end

fun len xs =
  case xs of 
       [] => 0
     | _::xs' => 1 + len xs'
