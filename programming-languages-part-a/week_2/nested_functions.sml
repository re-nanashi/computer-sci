fun count (from : int, to : int) =
  if from = to
  then to :: []
  else from :: count(from + 1, to)

fun countup_from1 (x : int) = 
  count(1, x)

fun count_l (x : int) = 
  let 
    fun count (from : int) = 
      if from = x
      then x :: []
      else from :: count(from + 1)
  in
    count(1)
  end

fun count_tailrec(x : int) =
  let 
    fun count (from : int, to : int, acc : int list) =
      if to = from
      then from :: acc
      else count(from, to - 1, from :: acc)
  in
    count(1, x, [])
  end
