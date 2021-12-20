(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = foldl (fn (x,y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" 

val longest_string_helper : (int * int -> bool) -> string list -> string = 
  fn f => fn str_list => foldl (fn (x,y) => 
    case f(String.size x, String.size y) of true =>x | false => y)
    "" str_list

val longest_string3 = longest_string_helper (fn (x,y) => x > y) 

val longest_string4 = longest_string_helper (fn (x,y) => x >= y) 

val longest_capitalized = longest_string3 o only_capitals

val rev_string = (String.implode o List.rev o String.explode) 

fun first_answer f xs = 
  case xs of 
       [] => raise NoAnswer 
     | (x::xs') => case f x of
                        NONE => first_answer f xs' 
                      | SOME v => v

fun all_answers f xs = 
  let fun aux (xs, acc) =
        case xs of 
             [] => SOME acc 
           | (x::xs') => case f x of 
                              NONE => NONE 
                            | SOME v => aux(xs', v @ acc)
  in aux(xs, []) end

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val count_wild_and_variable_lengths = g (fn _ => 1) String.size 

fun count_some_var (str, p) = g (fn _ => 0) (fn n => if n = str then 1 else 0) p

fun check_pat p = 
  let fun all_var_names p = 
        case p of 
             Variable x => [x] 
           | TupleP ps => List.foldl (fn (p, l) => (all_var_names p) @ l) [] ps 
           | ConstructorP (_, p) => all_var_names p 
           | _ => []
      fun no_duplicates ss = 
        let fun aux (ss, result) = 
              case ss of
                   [] => result
                 | (s::rest) => 
                     aux (rest, result andalso (not (List.exists (fn x => x = s) rest)))
        in aux(ss, true)
        end
    in (no_duplicates o all_var_names) p
    end

fun match (valu, ptrn) =  
  case (valu, ptrn) of
       (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s,v)]
     | (Unit, UnitP) => SOME []
     | (Const v, ConstP p) => if v = p then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if length vs = length ps 
                                then all_answers match (ListPair.zip(vs, ps))
                                else NONE
     | (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2 
                                                      then match(v,p)
                                                      else NONE
     | _ => NONE

fun first_match v ps = 
  SOME (first_answer (fn p => match(v, p)) ps)
  handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typecheck_patterns (dts, ps) = 
  let fun infer_type p = 
        case p of 
             UnitP => UnitT 
           | ConstP i => IntT 
           | TupleP ps => TupleT (List.map (fn p => infer_type p) ps) 
           | ConstructorP(s1, p1) => 
               let val p1_type = infer_type p1 
                   fun verify_datatype (cn,dn,t) = 
                     s1 = cn andalso 
                     (t = p1_type orelse p1_type = Anything) 
               in case List.find verify_datatype dts of 
                       NONE => raise NoAnswer 
                     | SOME (c,d,t) => Datatype d
               end 
           | _ => Anything 

      fun more_lenient_type (t1, t2) =
        case (t1, t2) of
             (Anything, t2) => t2
           | (t1, Anything) => t1
           | (IntT,   IntT) => IntT
           | (UnitT, UnitT) => UnitT
           | (TupleT ts1, TupleT ts2) =>
               if length ts1 = length ts2
               then TupleT (List.map more_lenient_type (ListPair.zip (ts1, ts2)))
               else raise NoAnswer
           | (Datatype dt1, Datatype dt2) => if dt1 = dt2
                                             then t1
                                             else raise NoAnswer
           | (_, _) => raise NoAnswer
  in
    case (dts, ps) of
         ([],[])    => NONE
       | (dts, ps)  => SOME (List.foldl (fn (t, acc) => more_lenient_type (t, acc)) 
                                         Anything (List.map infer_type ps)) 
                       handle NoAnswer => NONE
  end
