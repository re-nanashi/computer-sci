fun compose_opt f g a =
  case g a of
       NONE => NONE
     | SOME i => case f i of
                      NONE => NONE
                    | SOME v => SOME v

fun do_until f p x = 
  if p x = false
  then x
  else do_until f p (f x)

fun fixed_point f x = 
  do_until f (fn y => f y <> x) x

fun map2 f (x,y) = 
  (f x, f y)

fun app_all f g x = List.foldl (fn (lst, n_lst) => n_lst @ lst) [] (List.map f (g x))

fun foldr (f, init, xs) = 
  case xs of
       [] => init
     | (x::xs') => f (x, foldr(f, init, xs'))

fun partition f lst = 
  case lst of 
       [] => ([], [])
     | (x::xs) => let val (l1, l2) = partition f xs
                  in 
                    if f x 
                    then (x::l1, l2)
                    else (l1, x::l2)
                  end

fun unfold f a = 
  case f a of
       NONE => []
     | SOME (x, y) => x :: (unfold f y)

fun factorial n = 
  List.foldl (fn (x,y)=> x * y) 1 (unfold (fn n => if n = 0 then NONE else SOME (n, n - 1)) n)

fun map f xs = 
  List.foldr (fn (x, y) => (f x)::y) [] xs

fun foldl f a l = 
  let 
    fun g (x, f'') = fn y => f''(f(x, y)) 
  in 
    (List.foldr g (fn x => x) l) a 
  end
