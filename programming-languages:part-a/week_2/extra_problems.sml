(* alternate: int list -> int *)
(* add every number on the list with alternating signs *)
fun alternate (xs: int list) =
  let fun alternate_helper (xs : int list, index : int, sum : int) = 
        if null xs
        then sum
        else if index mod 2 <> 0 
        then alternate_helper (tl xs, index+1, sum + hd xs) 
        else alternate_helper (tl xs, index+1, sum - hd xs)
  in alternate_helper(xs, 1, 0)
  end

(* min_max: int list -> int * int *)
(* return a pair of the the minimum and maximum number in the list *)
fun min_max (xs : int list) =
  let fun min_max_helper (xs : int list, min : int, max : int) =
        if null xs
        then (min, max)
        else if hd xs > max 
        then min_max_helper(tl xs, min, hd xs)
        else if hd xs < min
        then min_max_helper(tl xs, hd xs, max)
        else min_max_helper(tl xs, min, max)
  in min_max_helper(xs, hd xs, hd xs)
  end

(* cumsum : int list -> int list *)
(* return a list of partial sums *)
fun cumsum (xs : int list) =
  let fun helper (xs : int list, c_sum : int, partial_sums : int list) =
        if null xs
        then partial_sums
        else helper(tl xs, c_sum + hd xs, partial_sums @ [(c_sum + hd xs)])
  in helper(xs, 0, [])
  end

(* repeat : int list * int list -> int list *)
(* return a list of repeated integers in the first list according to the second *)
fun repeat (xs : int list, ys: int list) = 
  let fun repeater (x : int, y : int, repeated_list : int list) =
        if y = 0
        then repeated_list
        else repeater (x, y - 1, x :: repeated_list)
        
      fun repeat_helper (xs : int list, ys : int list, repeated_list : int list) =
        if null xs orelse null ys
        then repeated_list
        else repeat_helper(tl xs, tl ys, repeated_list @ repeater(hd xs, hd ys, []))
  in repeat_helper(xs, ys, [])
  end

(* any : bool list -> bool *)
(* return true if there is atleast one element on the list that is true *)
fun any (bs : bool list) =
  if null bs
  then false
  else hd bs orelse any(tl bs)

(* all : bool list -> bool *)
(* return true if all elements on the list is true *)
fun all (bs : bool list) =
  if null bs
  then true
  else hd bs andalso all(tl bs)

(* zip : int list * int list -> int * int *)
(* return a list of consecutive pairs from the given lists *)
fun zip (list1 : int list, list2 : int list) =
  let fun zip_helper (list1 : int list, list2 : int list, 
                      c_list : (int * int) list) = 
        if null list1 orelse null list2
        then c_list
        else zip_helper (tl list1, tl list2, c_list @ [(hd list1, hd list2)])
  in zip_helper (list1, list2, [])
  end

fun zipRecycle list_double = 
  let val (l1, l2) = list_double
      fun zip_helper list_double = 
        case list_double of
             (_, []) => []
           | ([], l2) => zip_helper(l1, l2) 
           | (hd1::tl1, hd2::tl2) => (hd1, hd2) :: zip_helper(tl1, tl2)
  in 
    zip_helper list_double
  end

fun lookup losi_s =
  case losi_s of
       ([], _) => NONE
     | ((s,i)::tl, str) => if s = str then SOME i else lookup(tl, str)

fun splitup lon = 
  case lon of
       [] => ([], [])
     | (hd::tl) => let val (l1, l2) = splitup tl
                   in 
                     case hd >= 0 of
                          true => (hd::l1, l2)
                        | _ => (l1, hd::l2)
                   end

fun isSorted xs =
  case xs of 
       [] => true
     | _::[] => true
     | (head::(neck::rest)) => head <= neck andalso isSorted(neck::rest)

fun isAnySorted xs =
  case xs of 
       [] => true
     | _::[] => true
     | (head::(neck::rest)) => 
         let val increasing = head > neck 
         in case increasing of 
                true => head <= neck andalso isAnySorted(neck::rest)
              | _ => head >= neck andalso isAnySorted(neck::rest)
         end
fun sortedMerge double_list =
  case double_list of
       ([], []) => []
     | (ls, []) => ls
     | ([], ls) => ls
     | (hd1::tl1, hd2::tl2) => 
         if hd1 < hd2 
         then hd1 :: sortedMerge(tl1, hd2::tl2) 
         else hd2 :: sortedMerge(hd1::tl1, tl2)

