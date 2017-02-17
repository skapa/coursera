(* hw #1 solution *)

(* fun is_older takes two dates and returns if first date is older than second *)
fun is_older(date1: int*int*int, date2: int*int*int) =
  #1 date1 < #1 date2 orelse
  (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
  (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)
								      
(* fun number_in_month takes list of dates and month and returns number of dates in given month *)
fun number_in_month(dates : (int*int*int) list, month: int) =
  if null dates
  then 0
  else
      let fun test_date_in_month(tested_date_month: int) = 
	    if tested_date_month = month
	    then 1
	    else 0
      in
	  test_date_in_month(#2 (hd dates)) + number_in_month(tl dates, month)
      end

(* fun number_in_month takes list of dates and list of months and returns number of dates in any month from list *)
fun number_in_months(dates : (int*int*int) list, months: int list) =
  if null months orelse null dates
  then 0
  else
      number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* fun dates_in_month takes list of dates and month and returns list of dates in given month *)
fun dates_in_month(dates : (int*int*int) list, month: int) =
  if null dates
  then []
  else
      if #2 (hd dates) = month
      then hd dates :: dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

(* fun takes list of dates and list of months and returns list of dates in any of given months *)
fun dates_in_months(dates : (int*int*int) list, months: int list) =
  if null months orelse null dates
  then []
  else
      dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* fun takes list of strings and int and returns nth string *)  
fun get_nth(strings: (string) list, n: int) = 
  if n = 1
  then hd strings
  else get_nth(tl strings, n-1)

(* fun takes date and returns it's string representation in format: October 8, 1978 *)
fun date_to_string(date: (int*int*int)) =
let val months = ["January", "February", "March", "April",
		   "May", "June", "July", "August", "September", "October", "November", "December"];
in
    get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
end

(* fun takes int sum and list of ints and returns max number of members from list whose sum up don't exceed the sum *)
fun number_before_reaching_sum(sum: int, numbers: int list) =
  if sum - (hd numbers) <= 0
  then 0
  else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

(* fun takes int representing day in year and returnes int of month where day belong *)
fun what_month(day: int) = 
  let val month_days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, month_days) + 1
  end

(* fun takes two days and returnes list representing months where days from day1 to day2 belong *)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else
      if day1 = day2
      then [what_month day1]
      else what_month day1 :: month_range(day1 + 1, day2)

(* fun takes list of dates and returns the oldest one *)
fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else
      let fun get_older(dates: (int*int*int) list, oldest: (int*int*int)) =
	    if null dates
	    then oldest
	    else
		if is_older(hd dates, oldest)
		then get_older(tl dates, hd dates)
		else get_older(tl dates, oldest)
      in
	  SOME (get_older(tl dates, hd dates))
      end
