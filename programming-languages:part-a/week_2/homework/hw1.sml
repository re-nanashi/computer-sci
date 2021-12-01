(* (int * int * int) * (int * int * int) -> Bool *)
(* produce true if first argument is a date that comes before the second argument*)
fun is_older (date1 : (int * int * int), date2 : (int * int * int)) = 
  let fun date_in_days(date: (int * int * int)) =
      (#1 date * 365) + (#2 date * 12) + #3 date
  in
    date_in_days(date1) < date_in_days(date2)
  end

(* (int * int * int) list * int -> int *)
(* produce the number of dates in the list that are from the given month *)
fun number_in_month (dates : (int * int * int) list, month : int) =
  let fun get_number_of_dates (dates : (int * int * int) list, counter : int) =
    if null dates
    then counter
    else if #2 (hd dates) = month
    then get_number_of_dates(tl dates, (counter + 1))
    else get_number_of_dates(tl dates, counter)
  in
    get_number_of_dates (dates, 0)
  end

(* (int * int * int) list * int list -> int *)
(* produce the number of dates in the list that are from the given months *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  let 
    fun is_a_member (month : int, list_of_months : int list) =
      if null list_of_months
      then false
      else (month = (hd list_of_months)) orelse (is_a_member(month, tl list_of_months))

    fun get_number_of_dates (dates : (int * int * int) list, counter : int) =
      if null dates
      then counter
      else if is_a_member(#2 (hd dates), months)
      then get_number_of_dates(tl dates, (counter + 1))
      else get_number_of_dates(tl dates, counter)
  in
    get_number_of_dates(dates, 0)
  end

(* (int * int * int) list * int -> (int * int *int) list *)
(* produce a list of dates from the given list that is from the given month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  let 
    fun reverse_list (dates : (int * int * int) list) = 
      let 
        fun reverse_helper ((dates : (int * int * int) list), reversed : (int * int * int) list) = 
          if null dates
          then reversed
          else reverse_helper(tl dates, hd dates :: reversed)
      in
        reverse_helper(dates, [])
      end
      
    fun get_number_of_dates (dates : (int * int * int) list, new_list : (int * int * int) list) =
      if null dates
      then reverse_list (new_list)
      else if #2 (hd dates) = month
      then get_number_of_dates(tl dates, hd dates :: new_list)
      else get_number_of_dates(tl dates, new_list)
  in
    get_number_of_dates (dates, [])
  end

(* (int * int * int) list * int list -> (int * int *int) list *)
(* produce a list of dates from the given list that is from the given months *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  let 
    fun reverse_list (dates : (int * int * int) list) = 
      let 
        fun reverse_helper ((dates : (int * int * int) list), reversed : (int * int * int) list) = 
          if null dates
          then reversed
          else reverse_helper(tl dates, hd dates :: reversed)
      in
        reverse_helper(dates, [])
      end

    fun is_a_member (month : int, list_of_months : int list) =
      if null list_of_months
      then false
      else (month = (hd list_of_months)) orelse (is_a_member(month, tl list_of_months))

    fun get_number_of_dates (dates : (int * int * int) list, new_list : (int * int * int) list) =
      if null dates
      then reverse_list (new_list)
      else if is_a_member (#2 (hd dates), months) 
      then get_number_of_dates(tl dates, hd dates :: new_list)
      else get_number_of_dates(tl dates, new_list)
  in
    get_number_of_dates (dates, [])
  end
