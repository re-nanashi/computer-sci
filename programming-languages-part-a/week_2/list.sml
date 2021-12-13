(* int list -> int *)
(* produce the sum of every element in the list*)
fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)

val test1 = sum_list([1,2,3]) = 6

(* int list * int -> int *)
(* HELPER: produce the sum of every element in the list *)
fun sum_list_acc_helper(xs : int list, acc : int) =
  if null xs
  then acc
  else sum_list_acc_helper(tl xs, (acc + (hd xs)))

(* int list -> int *)
(* produce the sum of every element in the list *)
fun sum_list_acc(xs : int list) = 
  sum_list_acc_helper(xs, 0)

val test2 = sum_list_acc([1,2,3]) = 6

(* int list -> int *)
(* produce the product of every element in the list *)
fun list_product (xs : int list) =
  if null xs
  then 1
  else hd xs * list_product(tl xs)

val test3 = list_product([2,4,2]) = 16

(* int list -> int *)
(* HELPER: produce the product of every element in the list *)
fun list_product_acc_helper (xs : int list, acc : int) =
  if null xs
  then acc
  else list_product_acc_helper(tl xs, (acc * hd xs))

(* int list -> int *)
(* produce the product of every element in the list *)
fun list_product_acc (xs : int list) = 
  list_product_acc_helper(xs, 1)

val test4 = list_product_acc([2,4,2]) = 16

(* int -> int list *)
(* produce a list that is a countdown from given to 1 *)
fun countdown (x : int) = 
  if x = 0
  then []
  else x :: countdown(x - 1)

val test5 = countdown 7 = [7,6,5,4,3,2,1]

(* int * int list -> int list *)
fun countdown_acc_helper (x : int, acc : int list) =
  if x = 0
  then acc
  else countdown_acc_helper (x - 1, x :: acc)

(* int -> int list *)
fun countdown_acc (x : int) =
  countdown_acc_helper (x, [])

val test6 = countdown 7 = [7,6,5,4,3,2,1]

fun append (xs : int list, ys: int list) =
  if null xs
  then ys
  else hd xs :: append(tl xs, ys)

fun reverse_list_helper (xs: int list, acc: int list) =
  if null xs
  then acc
  else reverse_list_helper(tl xs, hd xs :: acc)

fun reverse_list (xs: int list) =
  reverse_list_helper(xs, [])

fun sum_pair_list (xs : (int * int ) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun append_acc_helper (xs : int list, ys: int list, acc: int list) =
  if null xs
  then acc
  else append_acc_helper (tl xs, ys, (hd xs :: acc))

fun append_acc (xs : int list, ys : int list) = 
  append_acc_helper(reverse_list xs, ys, ys)

fun firsts (xs: (int * int) list) = 
  if null xs
  then []
  else #1 (hd xs ) :: firsts(tl xs)

fun seconds (xs: (int * int) list) = 
  if null xs
  then []
  else #2 (hd xs ) :: seconds(tl xs)

fun sum_pair_list2(xs : (int * int) list) =
  (sum_list (first xs)) + (sum_list (seconds xs))
