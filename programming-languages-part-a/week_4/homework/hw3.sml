(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

fun only_capitals str_list =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) str_list

fun longest_string1 str_list = 
  foldl (fn (x,y) => if String.size y < String.size x then x else y) "" str_list

fun longest_string2 str_list = 
  foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" str_list

val longest_string_helper : (int * int -> bool) -> string list -> string = 
  fn f => fn str_list => foldl (fn (x,y) => 
    case f(String.size x, String.size y) of true =>x | false => y)
      "" str_list

val longest_string3 = longest_string_helper (fn (x,y) => y < x) 

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

(* NOTE: There is a big chance this code is buggy af. *)
fun typecheck_patterns (ts, ps) = 
  let fun match_challenge (tp, ptrn) =
        case (tp, ptrn) of
             (_, Wildcard)       => SOME []
           | ((_, _, t), Variable s)           => SOME[(s, t)]
           | ((_, _, UnitT), UnitP)            => SOME []
           | ((_, _, IntT), ConstP p)          => SOME []
           | ((con, dt, TupleT ts), TupleP ps) => 
               if length ts = length ps 
               then all_answers match_challenge 
                    (ListPair.zip((map (fn t => (con, dt, t)) ts), ps)) 
               else NONE
           | ((con,dt,t), ConstructorP (s, p)) => 
               if con = s 
               then match_challenge((con,dt,t), p) 
               else NONE
           | _ => NONE

    fun count_anything t = 
      let fun g f t = 
            let val r = g f 
            in case t of 
                    Anything  => f () 
                  | TupleT ts => List.foldl (fn (t,i) => (r t) + i) 0 ts 
                  | _         => 0 
            end
      in g (fn _ => 1) t
      end

    fun most_lenient (t::ts) =
      foldl (fn (x,y) => if count_anything x > count_anything y 
                         then x 
                         else y) t ts
  in 
    SOME (most_lenient 
          (List.map (fn (_,_,t) => t) 
          (List.filter (fn t => 
           List.all (fn p => case match_challenge (t, p) of 
                                  NONE => raise NoAnswer 
                                | SOME i => true) ps) ts))) 
    handle NoAnswer => NONE
    
  end
