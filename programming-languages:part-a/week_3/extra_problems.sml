(* Problems 1-4 use these type definitions: *)

(* StudentID is an Integer *)
(* interp. a student's id number *)
type student_id = int
(*
fun fn_for_student(id : int) =
  ... id
*)

(* Grade is Integer[0, 100] *)
(* interp. the range of grades of students *)
type grade = int 
(*
fun fn_for_grade(grade : int) =
  ... grade
*)

(* interp. a student's final grade *)
type final_grade = { id: student_id, grade : grade option }
(*
fun fn_for_fg{id = x, grade = y} =
  ... x         // int
      y         // int option
*)

(* Status is one of: pass | fail *)
datatype pass_fail = pass | fail

(* final_grade -> Status *)
(* return pass if the field contains SOME i for an i >= 75, otherwise fail *)
fun pass_or_fail {id = x, grade = y} = 
  case y of
       NONE => fail 
     | SOME i => 
         case i >= 75 andalso i <= 100 of 
              true => pass 
            | false => fail

(* final_grade -> bool *)
(* return true if and only if the grade field contains SOME i *)
fun has_passed {id = x, grade = y} =
  pass_or_fail({id = x, grade = y}) = pass

(* (listof final_grade) -> int*)
(* given a list of final_grade, return the number of passing student *)

fun number_passed lofg = 
  let fun number_passed_aux (lofg, count) = 
        case lofg of 
             [] => count 
           | (fg::rest) => 
               case has_passed(fg) of 
                    true => number_passed_aux(rest, count + 1) 
                  | _ => number_passed_aux(rest, count)
  in number_passed_aux(lofg, 0)
  end

(* (pass_fail * final_grade) list -> int *)
(* return the number of misgraded items in the list *)
(* get the final grade then call pass_or_fail then compare to given *)
fun number_misgraded lofg =
  let fun aux (lofg, count) =
        case lofg of
             [] => count
           | ((l, fg)::rest) => 
               case pass_or_fail(fg) <> l of 
                    true => aux(rest, count + 1)
                  | _ => aux(rest, count)
  in aux(lofg, 0)
  end

(* Problems 5-7 use these type definitions: *)

(* Tree is one of: 
 * - Leaf
 * - Node {value: 'a, left : 'a Tree, right : 'a Tree}
 * *)
datatype 'a tree = Leaf 
                 | Node of { value : 'a, left : 'a tree, right : 'a tree }

datatype flag = leave_me_alone | prune_me

val tr12 = Node {value=12, left=Leaf, right=Leaf}
val tr7  = Node {value=7, left=Leaf, right=Leaf}
val tr10 = Node {value=10, left=tr7, right=tr12}
val tr90 = Node {value=90, left=Leaf, right=Leaf}
val tr45 = Node {value=45, left=tr10, right=tr90}

(* 'a tree -> int *)
(* return the height of the tree *)
fun tree_height tree =
  let fun aux (tree, height) =
        case tree of
             Leaf => height
           | Node {value=x,left=lt,right=rt} => 
               Int.max(aux(lt, height + 1), aux(rt, height + 1))
  in aux(tree, 0)
  end

(* int tree -> int *)
(* produce the sum of all values of the tree *)
fun sum_tree i_tree =
  case i_tree of
       Leaf => 0
     | Node {value=x,left=lt,right=rt} =>
         x + sum_tree(lt) + sum_tree(rt)
