(* type is int list -> int *)
(* int because the function uses "+" *)
fun sum_list xs = 
  case xs of
       [] => 0
     | x::xs' => x + sum_list xs'

(* type is 'a list -> 'a list -> 'a list *)
fun append (xs, ys) = 
  case xs of 
       [] => ys
     | x::xs' => x :: append(xs', ys)

(* Polymorphic datatypes *)
datatype 'a option = NONE | SOME of 'a

datatype 'a mylist = Empty | Cons of 'a * 'a mylist

datatype ('a, 'b) tree =
        Node of 'a * ('a, 'b) tree * ('a, 'b) tree 
      | Leaf of 'b

(* type is (int, int) tree -> int *)
fun sum_tree (tr) = 
  case tr of 
       Leaf i => i
     | Node(i, left, right) => i + sum_tree left + sum_tree right

(* type is ('a, int) tree -> int *)
fun sum_leaves (tr)= 
  case tr of 
       Leaf i => i
     | Node(i, left, right) => sum_leaves left + sum_leaves right

(* type is ('a, 'b) tree -> int *)
fun num_leaves (tr) =
  case tr of 
       Leaf i => 1
     | Node(i, left, right) => num_leaves left + num_leaves right
