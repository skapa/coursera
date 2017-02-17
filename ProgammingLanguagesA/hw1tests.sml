use "hw1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
					
val test_f1_1 = is_older((1,1,1),(2,1,1)) = true
val test_f1_2 = is_older((1,1,1),(1,2,1)) = true
val test_f1_3 = is_older((1,1,1),(1,1,2)) = true
val test_f1_4 = is_older((1,1,1),(1,1,1)) = false
val test_f1_5 = is_older((1,1,2),(1,1,1)) = false
val test_f1_6 = is_older((1,2,1),(1,1,1)) = false
val test_f1_7 = is_older((2,1,1),(1,1,1)) = false
val test_f1_8 = is_older((2,1,1),(1,2,1)) = false
val test_f1_9 = is_older((2,1,1),(1,1,2)) = false
val test_f1_10 = is_older((2,1,2),(2,1,1)) = false

val test_f2_1 = number_in_month([], 1) = 0
val test_f2_2 = number_in_month([(1,1,1)], 1) = 1
val test_f2_3 = number_in_month([(1,1,1)], 2) = 0
val test_f2_4 = number_in_month([(1,1,1),(1,2,1),(2,1,2)], 1) = 2
val test_f2_5 = number_in_month([(1,2,1),(1,5,1),(2,7,2)], 1) = 0

val test_f3_1 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[]) = 0
val test_f3_2 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[1]) = 1
val test_f3_3 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[2]) = 2
val test_f3_4 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[1,2]) = 3
val test_f3_5 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[1,2,3]) = 5
val test_f3_6 = number_in_months([(1,1,1),(1,2,1),(1,3,1),(1,2,3),(1,3,1)],[6,5,3,1]) = 3

val test_f4_1 = dates_in_month([], 1) = []
val test_f4_2 = dates_in_month([(1,1,1)], 1) = [(1,1,1)]
val test_f4_3 = dates_in_month([(1,1,1)], 2) = []
val test_f4_4 = dates_in_month([(1,1,1),(1,2,1),(2,1,2)], 1) = [(1,1,1),(2,1,2)]
val test_f4_5 = dates_in_month([(1,2,1),(1,5,1),(2,7,2)], 1) = []

val test_f5_1 = dates_in_months([], [1,2,3]) = []
val test_f5_2 = dates_in_months([(1,1,1)], [1,2,3]) = [(1,1,1)]
val test_f5_3 = dates_in_months([(1,1,1)], [2]) = []
val test_f5_4 = dates_in_months([(1,1,1),(1,2,1),(2,1,2)], [1]) = [(1,1,1),(2,1,2)]
val test_f5_5 = dates_in_months([(1,2,1),(1,5,1),(2,7,2)], [1,3,4]) = []
val test_f5_6 = dates_in_months([(1,1,1),(1,2,1),(2,1,2),(1,2,3),(2,3,4)], [4,1,2]) = [(1,1,1),(2,1,2),(1,2,1),(1,2,3)]

val test_f6_1 = get_nth(["a","b","c"], 1) = "a"
val test_f6_2 = get_nth(["a","b","c"], 2) = "b"
val test_f6_3 = get_nth(["a","b","c"], 3) = "c"
						(* val test_f6_4 = get_nth(["a","b","c"], 4) = "" *)
val test_f7_1 = date_to_string((2017,1,22)) = "January 22, 2017";
val test_f7_2 = date_to_string((1978,10,8)) = "October 8, 1978";

val test_f8_1 = number_before_reaching_sum(1, [1,2,3,4,5,6,7,8,9,10]) = 0
val test_f8_2 = number_before_reaching_sum(7, [1,2,3,4,5,6,7,8,9,10]) = 3
val test_f8_3 = number_before_reaching_sum(15, [1,2,3,4,5,6,7,8,9,10]) = 4

val test_f9_1 = what_month 12 = 1
val test_f9_2 = what_month 32 = 2
val test_f9_3 = what_month 31 = 1
val test_f9_4 = what_month 59 = 2
val test_f9_5 = what_month 60 = 3
				    
val test_f10_1 = month_range(29,33) = [1,1,1,2,2]

val test_f11_1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28),(2010,1,1)]) = SOME (2010,1,1)
val test_f11_2 = oldest([]) = NONE
