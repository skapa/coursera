(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(needle, haystack) =
  let fun filter (xs, acc, needle_found) =
	case (xs, needle_found) of
	    ([], false) => NONE
	  | ([], true) => SOME acc
	  | (x::xs', needle_found) => (case same_string(x, needle) of
					   true => filter(xs', acc, true)
					 | false => filter(xs', acc @ [x], needle_found))
  in
      filter(haystack, [], false)
  end

fun get_substitutions1(substitutions, s) =
  case substitutions of
      [] => []
    | x::xs' => (case (xs', all_except_option(s, x)) of
		   (xs', NONE) => get_substitutions1(xs', s)
		   | (xs', SOME r) => r @ get_substitutions1(xs', s))

fun get_substitutions2(substitutions, s) =
  let fun aux(subs, acc) =
	case subs of
	    [] => acc
	  | x::xs' => (case all_except_option(s, x) of
			   NONE => aux(xs', acc)
			 | SOME r => aux(xs', acc @ r))
  in
      aux(substitutions, [])
  end

fun similar_names(substitutions, {first=f, middle=m, last=l}) =
  let fun aux(subs, acc) =
	case subs of
	    [] => acc
	  | x::xs' => aux(xs',acc@[{first=x, middle=m, last=l}])
  in
      aux(get_substitutions2(substitutions, f), [{first=f, middle=m, last=l}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color (suit, rank) =
  case suit of
      Clubs => Black
    | Spades => Black
    | Diamonds => Red
    | Hearts => Red

fun card_value (suit, rank) =
  case rank of
      Ace => 11
    | Num i => i
    | _ => 10

fun remove_card (cs, c, e) =
  let fun aux(cs, acc, c_found) =
	case (cs, c_found) of
	    ([], false) => raise e
	  | ([], true) => acc
	  | (x::xs', true) => aux(xs', acc@[x], true)
	  | (x::xs', false) => (case x=c of
				    true => aux(xs', acc, true)
				  | false => aux(xs', acc@[x], false))
  in
      aux(cs, [], false)
  end

fun all_same_color (cs) =
  case cs of
      [] => true
    | c::[] => true
    | x::y::xs' => (case card_color(x) = card_color(y) of
				      true => all_same_color(y::xs')
				    | false => false)

fun sum_cards(cs) =
  let fun aux(cs, acc) =
	case cs of
	    [] => acc
	  | x::xs' => aux(xs', acc + (card_value x))
  in
      aux(cs, 0)
  end

fun score (held_cards, goal) =
  case (sum_cards(held_cards), sum_cards(held_cards) > goal, all_same_color(held_cards)) of
	   (sum, true, true) => 3*(sum - goal) div 2
	 | (sum, true, false) => 3*(sum - goal) 
	 | (sum, false, true) => (goal - sum) div 2
	 | (sum, false, false) => (goal - sum)

fun officiate (cs, ms, goal) =
  let fun aux(held_cards, cards, moves) =
	case (held_cards, moves) of
	    (_, []) => score(held_cards, goal)
	  | (held_cards, m::ms') => (case (held_cards, cards, m) of
					 ([], _, Discard c) => raise IllegalMove
				       | (held_cards, cards, Discard c) => aux(remove_card(held_cards, c, IllegalMove), cards, ms')
				       | (held_cards, [], Draw) => score(held_cards, goal)
				       | (held_cards, c::cs', Draw) => (case sum_cards(c::held_cards) > goal of
									    true => score(c::held_cards, goal)
									  | false => aux(c::held_cards, cs', ms')))
  in
      aux([], cs, ms)
  end
      
