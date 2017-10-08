use "hw1.sml";

val test1 = is_older((2017,9,10),(2018,8,16)) = true
						    
val test2 = is_older((2019,9,10),(2018,8,16)) = false

val test3 = is_older((2017,9,10),(2017,10,16)) = true						    
val test4 = is_older((2017,9,10),(2017,8,16)) = false

val test5 = is_older((2017,9,10),(2017,9,16)) = true

val test6 = number_in_month([(2017,3,5),(2018,3,5),(2017,6,5), (20,3,10), (2017,8,5)],3) = 3

val test7 = number_in_months([(2017,3,5),(2018,4,5),(2017,6,5), (20,6,10), (2017,8,5)], [3]) = 1       

val test8 = number_in_months([(2017,3,5),(2018,4,5),(2017,6,5), (20,6,10), (2017,8,5)], [3,4]) = 2       

val test9 = number_in_months([(2017,3,5),(2018,4,5),(2017,6,5), (20,6,10), (2017,8,5)], [3,4,6]) = 4       
												   
val test10 = dates_in_month([(2017,3,5),(2018,3,5),(2017,6,5), (20,3,10), (2017,8,5)],3) = [(2017,3,5),(2018,3,5),(20,3,10)]

val test11 = dates_in_months([(2017,3,5),(2018,4,5),(2017,6,5), (20,6,10), (2017,8,5)], [3,4,6]) =  [(2017,3,5),(2018,4,5),(2017,6,5),(20,6,10)]

val test12 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 0) = ""											      
val test13 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 1) = "hello"

val test14 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 2) = "2nd"															      
val test15 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 2) = "2nd"

val test16 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 3) = "3rd"
								      
val test17 =  get_nth(["hello", "2nd", "3rd", "4th", "5th"], 5) = "5th"
								      
val test18 =  date_to_string(2017, 10, 06) = "October 6, 2017"

val test19 = number_before_reaching_sum(1,[1,2,3,4,5,6]) = 0
							       
val test20 = number_before_reaching_sum(4,[1,2,3,4,5,6]) = 2
val test21 = number_before_reaching_sum(8,[1,2,3,4,5,6]) = 3
val test22 = number_before_reaching_sum(12,[1,2,3,4,5,6]) = 4
val test23 = number_before_reaching_sum(21,[1,2,3,4,5,6]) = 5
val test24 = number_before_reaching_sum(2,[1,2,3,4,5,6]) = 1								
val test25 = what_month(10) = 1
val test26 = what_month(31) = 1
val test27 = what_month(40) = 2
val test28 = what_month(60) = 3
val test29 = what_month(90) = 3
val test30 = what_month(111) = 4

val test31 = month_range(2,6) = [1,1,1,1,1]
val test32 = month_range(57,60) = [2,2,2,3]
val test33 = month_range(10,8) = []
				     
val test34 =  oldest([]) = NONE
val test35 =  oldest([(2017, 4, 2)]) = SOME (2017,4,2)
val test36 =  oldest([(2017, 4, 2), (2017,4,1)]) = SOME (2017,4,1)
val test37 =  oldest([(2017, 3, 2), (2017,4,1)]) = SOME (2017,3,2)
val test38 =  oldest([(2017, 3, 12), (2017,4,1),(2017,3,5),(2018,3,5),(2017,6,5), (2017,3,10), (2017,8,5)]) = SOME (2017,3,5)			       
