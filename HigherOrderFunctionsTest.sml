use "hw3.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = only_capitals ["he", "Ankan", "small"] = ["Ankan"]
						 
val test3 = longest_string1 ["A","bc","C"] = "bc"
val test4 = longest_string1 ["Abc","abcd","aC"] = "abcd"
val test5 = longest_string1 ["left1","right","aC"] = "left1"

val test6 = longest_string2 ["A","bc","C"] = "bc"
val test7 = longest_string2 ["Abc","abcd","aC"] = "abcd"
val test8 = longest_string2 ["left1","right","aC"] = "right"

val test9 = longest_string3 ["A","bc","C"] = "bc"
val test10 = longest_string3 ["Abc","abcd","aC"] = "abcd"
val test11 = longest_string3 ["left1","right","aC"] = "left1"

val test12 = longest_string4 ["A","bc","C"] = "bc"
val test13 = longest_string4 ["Abc","abcd","aC"] = "abcd"
val test14 = longest_string4 ["left1","right","aC"] = "right"

val test15 = longest_string2 ["A","bc","C"] = "bc"
val test16 = longest_string3 ["A","bc","C"] = "bc"
val test17 = longest_string4 ["A","B","C"] = "C"

val test18 = longest_capitalized ["A","bc","C"] = "A"
val test19 = longest_capitalized ["ank","bc","cell"] = ""
val test20 = longest_capitalized ["Ank","bc","Cell"] = "Cell"
						      
val test21 = rev_string "reverse" = "esrever"
val test22 = rev_string "hannah" = "hannah"

val test23 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test24 = ((first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5]; false) handle NoAnswer => true);

val test25 =  all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test26 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test27 = count_wildcards Wildcard = 1

val test28 = count_wild_and_variable_lengths (Variable("a")) = 1
val test29 = count_wild_and_variable_lengths (Variable("abc")) = 3
																      
val test30 = count_some_var ("x", Variable("x")) = 1
					    
val test31 = check_pat (Variable("x")) = true
val test32 = check_pat (TupleP[TupleP[Wildcard, Variable "x"], Variable("z")]) = true
val test33 = check_pat (TupleP[TupleP[Wildcard, Variable "x"], Variable("x")]) = false
			
val test34 = match (Const(1), UnitP) = NONE
val test35 = match (Const(1), Wildcard) = SOME []

val test36 = first_match Unit [UnitP] = SOME []
