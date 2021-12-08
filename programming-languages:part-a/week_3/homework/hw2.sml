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
       [] => true
     | c1::[] => true
     | c1::(c2::cs') => 
         card_color c1 = card_color c2 andalso all_same_color(c2::cs')

fun sum_cards cs =                    
  let fun sum_cards_helper (cs, acc) = 
        case cs of
             [] => acc
           | c1::cs' => sum_cards_helper(cs', card_value c1 + acc)
  in sum_cards_helper(cs, 0)
  end

fun score (cl, goal) =
  let val sum = sum_cards cl
  in (if sum >= goal then (3 * (sum - goal)) else (goal - sum)) div
     (if all_same_color cl then 2 else 1)
  end

fun officiate (card_list, move_list, goal) =
  let fun loop (deck, ml, current_hand) = 
        case ml of
             [] => score(current_hand, goal)
           | (Discard c)::rest => 
               loop(deck, rest, remove_card(current_hand, c, IllegalMove))
           | Draw::rest => 
               case deck of
                    [] => score(current_hand, goal)
                  | c::tail => if sum_cards(c::current_hand) > goal
                               then score(c::current_hand, goal)
                               else loop(tail, rest, c::current_hand)
  in loop(card_list, move_list, [])
  end

fun score_challenge (loc, goal) =
  let val sum = sum_cards loc
      fun best_score (loc, sum) =
        case loc of
             [] => 3 * (sum - goal)
           | (_, Ace)::rest => 
               if sum - 10 > goal
               then best_score(rest, sum - 10)
               else Int.min(goal - (sum - 10), 3 * (sum - goal))
           | _::rest => best_score(rest, sum)
  in (if sum >= goal then best_score(loc, sum) else (goal - sum)) div
     (if all_same_color loc then 2 else 1)
  end

fun officiate_challenge (card_list, move_list, goal) =
  let fun sum_cards_min cs= 
        let fun min_card_val (suit, rank) = 
              case rank of
                   Ace => 1
                 | Num x => x
                 | _ => 10

            fun sum_cards_min_aux (cs, sum) =
              case cs of
                   [] => sum
                 | c1::cs' => sum_cards_min_aux(cs', sum + min_card_val c1)
        in sum_cards_min_aux (cs, 0)
        end

      fun loop (deck, ml, current_hand) = 
        case ml of
             [] => current_hand
           | (Discard c)::rest => 
               loop(deck, rest, remove_card(current_hand, c, IllegalMove))
           | Draw::rest => 
               case deck of
                    [] => current_hand
                  | c::tail => if sum_cards_min(c::current_hand) > goal
                               then c::current_hand
                               else loop(tail, rest, c::current_hand)
  in score_challenge(loop(card_list, move_list, []), goal)
  end

fun careful_player (card_list, goal) = 
  let fun generate_careful_moves (deck, hand, moves) = 
        let fun card_to_remove (cards, goal) = 
              case cards of
                   [] => NONE
                 | (c::cs) => 
                     if sum_cards(remove_card(cards, c, IllegalMove)) = goal
                     then SOME c
                     else card_to_remove(cs, goal - card_value(c))
        in case deck of
               [] => moves
             | (c::cs) => 
                 if goal - sum_cards(hand) > 10
                 then generate_careful_moves(cs, c::hand, moves @ [Draw])
                 else if goal = sum_cards(hand)
                 then moves
                 else case card_to_remove (c::hand, goal) of
                           NONE => if sum_cards(c::hand) > goal then moves
                                   else generate_careful_moves(cs, c::hand,
                                   moves @ [Draw])
                         | SOME c => moves @ [Discard c, Draw]
        end
  in 
    generate_careful_moves (card_list, [], [])
  end
