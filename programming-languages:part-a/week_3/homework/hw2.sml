(* Programming Languages: Part A Homework 2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, str_list) =
  case str_list of
       [] => NONE
     | s::s' => if same_string(str, s)
                then SOME s'
                else case all_except_option(str, s') of
                          NONE => NONE
                        | SOME sl => SOME(s::sl)

fun get_substitutions1 (substitutions, str) =
  case substitutions of
       [] => []
     | s::s' => case all_except_option(str, s) of
                     NONE => get_substitutions1(s', str)
                   | SOME l => l @ get_substitutions1(s', str)

fun get_substitutions2 (substitutions, str) =
  let fun helper (substitutions, acc) =
        case substitutions of
             [] => acc
           | s::s' => case all_except_option(str, s) of
                           NONE => helper(s', acc)
                         | SOME l => helper(s', acc @ l)
  in helper(substitutions, [])
  end
       
fun similar_names (str_list, {first = f, middle = m, last = l}) =
  let fun similar_names_helper xs =
        case xs of
             [] => []
           | x::xs' => {first = x, middle = m, last = l} :: similar_names_helper(xs') 
  in 
    {first = f, middle = m, last = l} :: 
    similar_names_helper(get_substitutions2(str_list, f)) 
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, rank) =
  case suit = Clubs orelse suit = Spades of
       true => Black
     | _ => Red

fun card_value (suit, rank) =
  case rank of
       Num x => x
     | Ace => 11
     | _ => 10

fun remove_card (cs, c, e) =
  case cs of
       [] => raise e
     | c1::rest => if c1 = c  
                 then rest 
                 else c1 :: remove_card(rest, c, e)

fun all_same_color cs = 
  case cs of 
       [] => false
     | c1::[] => true
     | c1::(c2::cs') => if card_color c1 = card_color c2 
                        then all_same_color(c2::cs')
                        else false
fun sum_cards cs =                    
  let fun sum_cards_helper (cs, acc) = 
        case cs of
             [] => acc
           | c1::cs' => sum_cards_helper(cs', card_value c1 + acc)
  in sum_cards_helper(cs, 0)
  end
