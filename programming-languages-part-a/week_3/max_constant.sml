(* Section 2: Another Expressin Example *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun max_constant (e : exp) =
  case e of
       Constant i => i 
     | Negate (e2 )=> max_constant(e2)
     | Add (e1, e2)=> if max_constant(e1) > max_constant(e2)
                      then max_constant(e1)
                      else max_constant(e2)
     | Multiply (e1, e2)=> if max_constant(e1) > max_constant(e2)
                      then max_constant(e1)
                      else max_constant(e2)

(* alternative *)
fun max_constant (e : exp) =
  case e of
       Constant i => i 
     | Negate (e2)=> max_constant(e2)
     | Add (e1, e2)=> 
       let val m1 = max_constant(e1)
           val m2 = max_constant(e2)
       in if m1 > m2 then m1 else m2 end
     | Multiply (e1, e2)=> 
       let val m1 = max_constant(e1)
           val m2 = max_constant(e2)
       in if m1 > m2 then m1 else m2 end

(* alternative *)
fun max_constant (e : exp) =
  let fun max_of_two (e1, e2) = 
       let val m1 = max_constant(e1)
           val m2 = max_constant(e2)
       in if m1 > m2 then m1 else m2 end
  case e of
       Constant i => i 
     | Negate (e2)=> max_constant(e2)
     | Add (e1, e2)=> max_of_two(e1, e2)
     | Multiply (e1, e2)=> max_of_two(e1, e2)

(* alternative : BEST*)
fun max_constant (e : exp) =
  case e of
       Constant i => i 
     | Negate (e2)=> max_constant(e2)
     | Add (e1, e2)=> Int.max(max_constant(e1), max_constant(e2))
     | Multiply (e1, e2)=> Int.max(max_constant(e1), max_constant(e2))
