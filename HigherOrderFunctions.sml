exception NoAnswer
      
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu


fun g f1 f2 p =
  let 
      val r = g f1 f2 
  in
      case p of
	  Wildcard          => f1 ()
	| Variable x        => f2 x
	| TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	| ConstructorP(_,p) => r p
	| _                 => 0
  end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals xs =
  List.filter (fn x => (Char.isUpper o String.sub)(x, 0)) xs

(* 2 *)
fun longest_string1 xs =
  List.foldl (fn (x1,x2) => if String.size x1 > String.size x2
			 then x1
			 else x2) "" xs

(* 3 *)
fun longest_string2 xs =
  List.foldl (fn (x1,x2) => if String.size x1 >= String.size x2
			 then x1
			 else x2) "" xs
				
(* 4 *)
fun longest_string_helper f xs =
  List.foldl (fn (x1,x2) => if f(String.size x1, String.size x2)
			    then x1
			    else x2 ) "" xs

val longest_string3 = longest_string_helper (fn (x1,x2) =>  x1 > x2)
							   
			  
val longest_string4 = longest_string_helper (fn (x1,x2) => x1 >= x2)

(* 5 *)
val longest_capitalized =
  foldl (fn (x1,x2) => if (Char.isUpper o String.sub)(x1,0) andalso String.size x1 > String.size x2
		       then x1
		       else x2) ""
		
(* 6 *)
fun rev_string x =
  (String.implode o List.rev o String.explode) x

(* 7 *)
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs => case f x of
		   NONE => first_answer f xs
		 | SOME v => v

(* 8 *)
fun all_answers f xs =
  let
      fun loop acc f xs =
	case xs of
	    [] => SOME acc
	  | y::ys => case f y of
			 NONE => NONE
		       | SOME v => loop (acc@v) f ys 
  in
      loop [] f xs
  end
      
(* 9 *)      
fun count_wildcards p =
  g (fn () => 1) (fn _ => 0) p;

fun count_wild_and_variable_lengths p =
  g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s,p) =
  g (fn () => 0) (fn x => if s = x then 1 else 0) p

(* 10 *)
fun check_pat p=
  let fun all_var p =
	case p of
	    Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p,i) => i @ (all_var p)) [] ps
	  | ConstructorP(_,p) => all_var p
	  | _                 => []
      fun has_repeat xs =
	case xs of
	    [] => false
	  | x::xs => List.exists (fn y: string => x=y) xs orelse has_repeat xs 
  in
      not ((has_repeat o all_var) p)
  end

(* 11 *)
fun match (v,p) =
     case (v,p) of  
	 (_, Wildcard) => SOME []
	|(_, Variable s ) => SOME [(s,v)]
	|(Unit,UnitP) => SOME []
	|(Const v,ConstP p) => if v = p
			       then SOME []
			       else NONE
	|(Tuple vs, TupleP ps) => if List.length vs = List.length ps 
				  then case all_answers match (ListPair.zip(vs,ps))  of
					   SOME v => SOME v
					 | _ => NONE
				  else NONE
	|(Constructor (s, v),ConstructorP (s1, p) ) => if s = s1
						       then match(v, p)
						       else NONE
								
	|(_ , _) => NONE
		      

(* 12 *)
fun first_match v p =
  SOME (first_answer (fn x => match(v,x)) p)
  handle NoAnswer =>NONE
			
