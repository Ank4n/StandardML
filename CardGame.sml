fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* 1a Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. *)	   
fun all_except_option(x, xl) =
  case xl of
      [] => NONE
    | y::ys => if same_string(x,y) then SOME ys
	       else case all_except_option(x,ys) of
			NONE => NONE
		      | SOME z => SOME (y::z)
		   

(* 1b Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. *)
fun get_substitutions1(sll, s)=
  case sll of
      [] => []
    | x::xs => case all_except_option(s,x) of
		   NONE => get_substitutions1(xs,s)
		 | SOME x => x @ get_substitutions1(xs,s)
						    

(* 1c Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2(sll, s)=
  let
      fun aux(lst, acc) =
	case lst of
	    [] => acc
	  | x::xs =>
	    aux (xs, (case all_except_option(s,x) of
		   NONE =>  acc
		 | SOME x => acc @ x
		     )
		)
  in
      aux(sll, [])
  end

(* 1d Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). *)      
 fun similar_names(sll, {first=x, middle=y, last=z}) =
   let
       fun get_names(lst) =
	     case lst of
		 n::ns => {first = n, middle = y, last =z}::get_names(ns)
	       | _ => []
   in
	   {first=x, middle=y, last=z}::get_names(get_substitutions2(sll,x))
   end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(*2a*)
fun card_color c =
  case c of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (_,_) => Red

(*2b*)
fun card_value c =
  case c of
      (_, Num x) => x
    | (_,Ace) => 11
    | (_,_) => 10 

(*2c*)		   
fun remove_card (cs, c, e) =
  case cs of
      [] => raise e
    | x::xs => if (x=c) then  xs
	       else x::remove_card(xs, c, e)
			       
					   
(*2d*)
fun all_same_color cs =
  case cs of
      [] => true
    | x::[] => true
    | x1::x2::xs => if (card_color x1 <> card_color x2) then false
		    else all_same_color(x2::xs)

      
(*2e*)
fun sum_cards cs =
  let fun aux(acc, cs) =
	case cs of
	    [] => acc
	  | x::xs =>  aux(acc+card_value(x),xs)
  in
      aux(0,cs)
  end
      
(*2f*)
fun score (cs, goal) =
  let val sum = sum_cards(cs)
  in
      let val pscore = if sum>goal then 3*(sum-goal)
		       else (goal-sum)
      in
	  if (all_same_color(cs)) then round(real(pscore)/2.0)
	  else pscore
      end
  end
      
			 
      
(*2g*)
fun officiate (cs, mvs, goal) =
  let
      fun play(cs, mvs, hcs, sum) =
	 case  mvs  of
	     [] => score(hcs, goal)		 
	   | (Discard c)::xs => if (sum>goal) then score(hcs, goal)
				else
				    let val held_cards = remove_card(hcs, c, IllegalMove) in     
					play(cs, xs, held_cards, sum_cards(held_cards))
				    end
	   | Draw::xs => if (sum>goal) then score(hcs, goal)
			    else case cs of
				     [] => score(hcs, goal)
				   | y::ys  => play(ys, xs, y::hcs, sum_cards(y::hcs))
  in
      play(cs, mvs, [], 0)
  end
      
