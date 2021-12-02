(* Programming Languages: Part A Homework 1 *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) = 
  let fun convert_to_days(date: (int * int * int)) =
        (#1 date * 365) + (#2 date * 12) + #3 date
  in convert_to_days(date1) < convert_to_days(date2)
  end

fun number_in_month (dates : (int * int * int) list, month : int) =
  let fun number_in_month_helper (dates : (int * int * int) list, counter :
  int) =
        if null dates
        then counter
        else if (#2 (hd dates)) = month
        then number_in_month_helper(tl dates, counter + 1)
        else number_in_month_helper(tl dates, counter)
  in number_in_month_helper(dates, 0)
  end

fun number_in_months (dates : (int * int * int) list, months : int list) =
  let fun number_in_months_helper (months : int list, counter : int) =
        if null months
        then counter
        else 
          number_in_months_helper(tl months, counter + number_in_month(dates, hd months))
  in number_in_months_helper(months, 0)
  end

fun dates_in_month (dates : (int * int * int) list, month : int) =
  let fun dates_in_month_helper (dates : (int * int * int) list, 
    new_list : (int * int * int) list) =
        if null dates
        then new_list
        else if (#2 (hd dates)) = month
        then dates_in_month_helper(tl dates, new_list @ [hd dates])
        else dates_in_month_helper(tl dates, new_list)
  in dates_in_month_helper(dates, [])
  end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  let fun dates_in_months_helper (months : int list, 
    new_list : (int * int * int) list) =
        if null months
        then new_list
        else 
          dates_in_months_helper(tl months, new_list @ dates_in_month(dates, hd
          months))
  in dates_in_months_helper(months, [])
  end

fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string (date : (int * int * int)) =
  let val list_of_months = ["January","February","March","April","May",
                            "June","July","August","September","October",
                            "November","December"]
  in get_nth(list_of_months, #2 date) ^ " " ^ 
     Int.toString (#3 date) ^ ", " ^ 
     Int.toString (#1 date)
  end

fun number_before_reaching_sum (sum : int, list_of_numbers : int list) =
  let fun helper (list_of_numbers : int list, index : int, c_sum : int) =
        if c_sum + hd list_of_numbers >= sum
        then index
        else helper(tl list_of_numbers, index + 1, 
                    c_sum + hd list_of_numbers)
  in helper(list_of_numbers, 0, 0)
  end

fun what_month (day : int) =
  let val no_of_days_per_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum(day, no_of_days_per_month) + 1
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates: (int * int * int) list) =
  if null dates
  then NONE
  else let
    fun oldest_nonempty (dates: (int * int * int) list) =
      if null (tl dates)
      then (hd dates)
      else let val tl_ans = oldest_nonempty(tl dates)
           in if is_older((hd dates), tl_ans)
              then (hd dates)
              else tl_ans
           end
    in SOME(oldest_nonempty(dates))
    end

fun is_a_member_of(x : int, xs : int list) =
  if null xs
  then false
  else x = (hd xs) orelse is_a_member_of(x, tl xs)

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
  let fun number_in_months_helper (months : int list, seen : int list, counter : int) = 
        if null months 
        then counter 
        else if is_a_member_of((hd months), seen)
        then
          number_in_months_helper(tl months, seen, counter)
        else
          number_in_months_helper(tl months, hd months :: seen, counter + number_in_month(dates, hd months))
  in number_in_months_helper(months, [], 0)
  end

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
  let fun dates_in_months_helper (months : int list, seen: int list, new_list : (int * int * int) list) = 
        if null months
        then new_list
        else if is_a_member_of((hd months), seen)
        then
          dates_in_months_helper(tl months, seen, new_list)
        else
          dates_in_months_helper(tl months, (hd months) :: seen, new_list @ dates_in_month(dates, hd months))
  in dates_in_months_helper(months, [], [])
  end

fun reasonable_date(date : (int * int * int)) =
  let fun is_month_valid (month : int) =
        1 <= month andalso month <= 12

      fun is_year_valid (year : int) =
        0 < year

      fun is_day_valid (year : int, month : int, day : int) =
        let fun no_of_days_in_month (year : int, month : int) =
              let val is_leap_year = (year mod 4 = 0) andalso 
                                    ((year mod 400 = 0) orelse  (year mod 100 <> 0))
                  val february = if is_leap_year then 29 else 28
                  val days_in_a_month = [31,february,31,30,31,30,31,31,30,31,30,31]

                  fun get_days (month : int, list_of_months : int list) = 
                    if month = 1
                    then hd list_of_months
                    else get_days(month - 1, tl list_of_months)
              in 
                get_days(month, days_in_a_month)
              end
        in 1 <= day andalso day <= no_of_days_in_month(year, month)
        end

  in is_month_valid(#2 date) andalso 
     is_year_valid(#1 date) andalso  
     is_day_valid(#1 date, #2 date, #3 date)
  end
