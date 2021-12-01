fun is_older (date1 : (int * int * int), date2 : (int * int * int)) = 
  let fun convert_to_days(date: (int * int * int)) =
        (#1 date * 365) + (#2 date * 12) + #3 date
  in
    convert_to_days(date1) < convert_to_days(date2)
  end

fun number_in_month (dates : (int * int * int) list, month : int) =
  let fun number_in_month_helper ((dates : (int * int * int) list, counter :
  int)) =
        if null dates
        then counter
        else if (#2 (hd dates)) = month
        then number_in_month_helper(tl dates, counter + 1)
        else number_in_month_helper(tl dates, counter)
  in 
    number_in_month_helper(dates, 0)
  end

fun number_in_months (dates : (int * int * int) list, months : int list) =
  let fun number_in_months_helper (months : int list, counter : int) =
        if null months
        then counter
        else 
          number_in_months_helper(tl months, counter + number_in_month(dates, hd months))
  in
    number_in_months_helper(months, 0)
  end

fun dates_in_month (dates : (int * int * int) list, month : int) =
  let fun dates_in_month_helper (dates : (int * int * int) list, new_list : (int
   * int * int) list) =
        if null dates
        then new_list
        else if (#2 (hd dates)) = month
        then dates_in_month_helper(tl dates, new_list @ [hd dates])
        else dates_in_month_helper(tl dates, new_list)
  in
    dates_in_month_helper(dates, [])
  end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  let fun dates_in_months_helper (months : int list, 
    new_list : (int * int * int) list) =
        if null months
        then new_list
        else 
          dates_in_months_helper(tl months, new_list @ dates_in_month(dates, hd
          months))
  in 
    dates_in_months_helper(months, [])
  end

fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string (date : (int * int * int)) =
  let val list_of_months = ["January","February","March","April","May",
                            "June","July","August","September","October",
                            "November","December"]
  in
    get_nth(list_of_months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^
    Int.toString (#1 date)
  end


