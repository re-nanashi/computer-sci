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

val count_wild_and_variable_lengths = g (fn _ => 1) (fn n => String.size n)

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

fun match (v, p) = 
  case (v, p) of
       (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s,v)]
     | (Unit, UnitP) => SOME []
     | (Const n1, ConstP n2) => if n1 = n2 then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if length vs = length ps
                                then all_answers match (ListPair.zip (vs, ps))
                                else NONE
     | (Constructor (s1,v), ConstructorP (s2, pn)) => if s1 = s2 
                                                      then match(v,pn)
                                                      else NONE
     | _ => NONE

fun first_match v ps =
  SOME (first_answer (fn p => match (v, p)) ps)
  handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
