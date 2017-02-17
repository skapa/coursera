use "hw2.sml";
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []


val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)


val test_a_1 = all_except_option ("Istring", ["string"]) = NONE
val test_a_2 = all_except_option ("string", ["string", "foo", "bar"]) = SOME ["foo", "bar"]
val test_a_3 = all_except_option ("Istring", []) = NONE
val test_a_4 = all_except_option ("string", ["foo", "string", "bar"]) = SOME ["foo", "bar"]
val test_a_5 = all_except_option ("string", ["foo", "bar", "string"]) = SOME ["foo", "bar"]
val test_a_6 = all_except_option ("string", ["istring", "foo", "bar"]) = NONE

val test_b_1 = get_substitutions1 ([["foo", "fool"],["there"]], "foo") = ["fool"]
val test_b_2 = get_substitutions1 ([["foo", "fool", "foo2"],["there"], ["foom", "foo","fook","fool"], ["sss","bbb"]], "foo") = ["fool", "foo2", "foom", "fook", "fool"]
val test_b_3 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test_b_4 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test_c_1 = get_substitutions2 ([["foo", "fool"],["there"]], "foo") = ["fool"]
val test_c_2 = get_substitutions2 ([["foo", "fool", "foo2"],["there"], ["foom", "foo","fook","fool"], ["sss","bbb"]], "foo") = ["fool", "foo2", "foom", "fook", "fool"]
val test_c_3 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test_c_4 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test_d_1 = similar_names ([["foo", "fool"],["there"]], {first="foo", middle="W", last="Smith"}) =
	    [{first="foo", last="Smith", middle="W"}, {first="fool", last="Smith", middle="W"}]
val test_d_2 = similar_names([],{first="palo",middle="", last="druska"}) = [{first="palo", middle="", last="druska"}]
val test_d_3 = similar_names([["juro","jurko","duri"],["palo","palko","pali","palino"]],{first="lucka", middle="",last="druskova"}) = [{first="lucka",middle="",last="druskova"}]

val test_2a_1 = card_color (Hearts, Num 2) = Red
val test_2a_2 = card_color (Spades, Queen) = Black
val test_2a_3 = card_color (Diamonds, King) = Red

val test_2b_1 = card_value (Hearts, Num 2) = 2
val test_2b_2 = card_value (Spades, Ace) = 11
val test_2b_3 = card_value (Diamonds, King) = 10

val test_2c_1 = remove_card ([(Hearts, Ace), (Diamonds, Num 2), (Spades, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Num 2), (Spades, Ace)]
(*val test_2c_2 = (remove_card ([(Hearts, Ace), (Diamonds, Num 2), (Spades, Ace)], (Hearts, King), IllegalMove) handle IllegalMove => true)*)

val test_2d_1 = all_same_color([(Hearts, Num 2),(Hearts, Ace),(Hearts, King)]) = true
val test_2d_2 = all_same_color([(Diamonds, Ace)]) = true
val test_2d_3 = all_same_color([(Spades, Ace),(Spades, Queen), (Diamonds, Queen)]) = false
val test_2d_4 = all_same_color([(Hearts, Ace), (Diamonds, King)]) = true
											 
val test_2e_1 = sum_cards([(Diamonds, Num 2), (Spades, Num 7), (Hearts, King), (Clubs, Ace)]) = 30
val test_2e_2 = sum_cards([]) = 0
val test_2e_3 = sum_cards([(Diamonds, Ace)]) = 11
		    
val test_2f_1 = score([(Hearts, Ace)], 10) = 1
val test_2f_2 = score([(Hearts, Ace), (Clubs, Num 6)], 10) = 21
val test_2f_3 = score([(Hearts, Queen), (Hearts, King)], 22) = 1
val test_2f_4 = score([(Diamonds, Jack), (Clubs, Num 5)], 20) = 5

val test_2g_1 = officiate([],[], 5) = 2
val test_2g_2 = officiate([(Hearts, Ace), (Diamonds, Num 2), (Clubs, King)], [Draw, Draw, Discard (Diamonds, Num 2), Draw], 21) = 0 
