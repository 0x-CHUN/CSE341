fun is_older(d1 : int*int*int ,d2 : int*int*int) =
    if #1 d1 < #1 d2
    then true
    else if #1 d1 = #1 d2 andalso #2 d1 < #2 d2 then true
    else if #1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2 then true
    else false

fun number_in_month(dates : (int*int*int) list,month : int) =
    if null dates
    then 0
    else
        let val res = if #2 (hd dates) = month then 1 else 0
        in
            res + number_in_month(tl dates, month)
        end

fun number_in_months(dates : (int*int*int) list,months : int list) =
    if null months
    then 0
    else number_in_month(dates,hd months) + number_in_months(dates,tl months)

fun dates_in_month(dates : (int*int*int) list,month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates,month)
    else dates_in_month(tl dates,month)

fun dates_in_months(dates : (int*int*int) list,months : int list) =
    if null months
    then []
    else dates_in_month(dates,hd months)@dates_in_months(dates,tl months)

fun get_nth(lst : string list,n : int) =
    if n = 1
    then hd lst
    else get_nth(tl lst, n - 1)

fun date_to_string(date : int*int*int) =
    let val months =["January", "February", "March", "April",
            "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int,lst : int list) =
    if sum <= 0
    then ~1
    else 1 + number_before_reaching_sum(sum - (hd lst),tl lst)

fun what_month(day : int) =
    let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, days) + 1
    end

fun month_range(day1 : int,day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let val ret = oldest(tl dates)
        in
            if isSome ret andalso is_older(valOf ret, hd dates)
            then ret
            else SOME(hd dates)
        end