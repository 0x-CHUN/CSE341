use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3;

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test13 = number_in_months_challenge([(31, 12, 1888), (25, 9, 1999), (15, 7, 2099), (25, 9, 2008)], [7, 9, 8, 9]) = 3  andalso (number_in_months_challenge ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], []) = 0) andalso (dates_in_months_challenge([(31, 12, 1888), (25, 9, 1999), (15, 7, 2099), (25, 9, 2008)], [7, 9, 8, 9]) = [(15,7,2099), (25,9,1999), (25,9,2008)]) andalso (dates_in_months_challenge([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], []) = []);

val test14 = not (reasonable_date(23, 12, 0)) andalso (reasonable_date(23, 12, 31)) andalso (reasonable_date(2008, 2, 29)) andalso not (reasonable_date(1900, 2, 29));