(* Abstract Data Type Implementation *)

datatype set = S of { insert : int -> set,
                      member : int -> bool,
                      size   : unit -> int }

(*datatype set = S of { insert : int -> set, 
                        member : int -> bool,
                        size   : unit -> int }*)

(* Client *)
fun use_sets () = 
  let val S s1 = empty_set
      val S s2 = (#insert s1) 34 
      val S s3 = (#insert s2) 34
      val S s4 = (#insert s3) 19
  in if (#member s4) 42
     then 99
     else if (#member s4) 19
     then 17 + (#size s3) ()
     else 0
  end

