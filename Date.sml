(* year month date *)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
  if (#1 date1 <> #1 date2)
  then #1 date1 < #1 date2
  else if (#2 date1 <> #2 date2)
  then  #2 date1 < #2 date2
  else  #3 date1 < #3 date2
		     
fun number_in_month(dates : (int*int*int) list, month : int)=
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month((tl dates), month)
  else number_in_month((tl dates), month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
  if null dates orelse null months
  then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))	   


fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then dates
  else if #2 (hd dates) = month
  then (hd dates)::dates_in_month((tl dates), month)
  else dates_in_month((tl dates), month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null dates orelse null months
  then []
  else (dates_in_month(dates, (hd months))) @ (dates_in_months(dates, (tl months)))

fun get_nth(strings : string list, n : int) =
  if (n<0 orelse (null strings))
  then ""
  else if n=1
  then hd strings
  else get_nth(tl(strings), n-1)
	      
		     
fun date_to_string(date : int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end
      
fun number_before_reaching_sum(sum: int, list: int list) =
  if null list orelse (hd list) >= sum
  then 0
  else 1 + number_before_reaching_sum(sum-(hd list), tl list) 

fun what_month(day : int) =
  let
      val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, months) + 1
  end

fun month_range(d1 : int, d2 : int) =
  if d1>d2 then []
  else 
      what_month(d1)::month_range(d1+1,d2)
				  
fun oldest(dates : (int*int*int) list) =
  if null dates
  then NONE
  else if null (tl dates)
  then SOME (hd dates)
  else if is_older(hd dates, hd (tl dates))
  then oldest(hd dates :: tl (tl dates))
  else oldest(hd (tl dates) :: tl (tl dates))
			      
