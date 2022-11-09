(* Coursera Programming Languages, Homework 3, Provided Code *)

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

fun only_capitals(strs) =
	List.filter (fn s => Char.isUpper(String.sub(s, 0))) strs

val only_capitals_test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test2 = only_capitals ["A","b","C"] = ["A","C"]
val only_capitals_test3 = only_capitals ["A","B","c"] = ["A","B"]

fun longest_string1(strs) =
	List.foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) "" strs

val longest_string1_test1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test2 = longest_string1 ["ABB","bcc","C"] = "ABB"
val longest_string1_test3 = longest_string1 ["A","bcc","Ccc"] = "bcc"


fun longest_string2(strs) =
	List.foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) "" strs

val longest_string2_test1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test2 = longest_string2 ["ABB","bcc","C"] = "bcc"
val longest_string2_test3 = longest_string2 ["A","bcc","Ccc"] = "Ccc"

fun longest_string_helper f strs =
	List.foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) "" strs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string3_test1 = longest_string3 ["A","bc","C"] = "bc"
val longest_string3_test2 = longest_string3 ["ABB","bcc","C"] = "ABB"
val longest_string3_test3 = longest_string3 ["A","bcc","Ccc"] = "bcc"

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_string4_test1 = longest_string4 ["A","bc","C"] = "bc"
val longest_string4_test2 = longest_string4 ["ABB","bcc","C"] = "bcc"
val longest_string4_test3 = longest_string4 ["A","bcc","Ccc"] = "Ccc"


val longest_capitalized = longest_string1 o only_capitals

val longest_capitalized_test1 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test2 = longest_capitalized ["a","bc","C"] = "C"
val longest_capitalized_test3 = longest_capitalized ["a","bc","c"] = ""

val rev_string = String.implode o List.rev o String.explode

val rev_string_test1 = rev_string "abc" = "cba"
val rev_string_test2 = rev_string "" = ""
val rev_string_test3 = rev_string "ab" = "ba"

fun first_answer f xs =
	case ((List.filter isSome) o (List.map f)) xs of
		(SOME x)::xs' => x
	|	_ => raise NoAnswer

val first_answer_test1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val first_answer_test2 = first_answer (fn x => if x*x > 4 then SOME x else NONE) [1,2,3,4,5] = 3

fun all_answers f xs =
	let 
		val get_result = List.foldl (fn (x, acc) => (case x of 
														SOME v => acc@v
													|	_ => acc))
	in
		case ((List.filter isSome) o (List.map f)) xs of
			[] => NONE
		|	xs' => SOME(get_result [] xs')
	end
val all_answers_test1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val all_answers_test2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME([2,3,4,5,6,7])
val all_answers_test3 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME([2,4,6])


val count_wildcards = g (fn _ => 1)(fn _ =>0)
val count_wild_and_variable_lengths = g (fn _ => 1)(fn str => String.size str)
fun count_some_var (x,p) = g  (fn _ => 0)(fn str =>if x = str then 1 else 0) p

fun check_pat p = 
  let 
      fun get_str_list p = 
         case p of
            Variable x => [x] 
           |TupleP ps  => List.foldl (fn (r,i) => get_str_list(r)@i) [] ps
           | _ => []

      fun do_same_exists x = List.exists(fn y => x = y ) 
    
      fun check_uniqueness lst =
       case lst of
        [] => true
        | x::xs =>	if (do_same_exists x xs) then false
                    else check_uniqueness xs
  in
    check_uniqueness (get_str_list p)
  end

fun match (v,p)   =
	case (v,p) of
		(_,Wildcard) => SOME []
    |	(Const v1,ConstP p1) =>if v1 = p1 then SOME [] else NONE
    |	(Unit,UnitP) =>SOME []
    |	(Constructor (s ,v1),ConstructorP (s1, p1) ) => if s = s1 then match(v1,p1) else NONE
    |	(Tuple vs,TupleP ps) => if List.length vs = List.length ps 
									then case all_answers match (ListPair.zip(vs,ps)) of
										SOME v2=>SOME v2
                                   	|	_ => NONE
                              	else NONE
    |	(_, Variable s ) => SOME [(s,v)]
    |	(_,_) => NONE


fun first_match v p =
    SOME (first_answer (fn x => match(v,x)) p)
    handle NoAnswer =>NONE

fun get_pattern_type(type_data ,pattern) =
	let  
		fun check_type_data (type_data1, datatype_str,cons_type)  =
            case type_data1 of
                	[]=> raise NoAnswer
                |	(x,y,z)::xs => if x = datatype_str andalso cons_type = z then Datatype y
                                	else check_type_data(xs, datatype_str,cons_type)

   		fun helper ptn =
		case  ptn of
      			[] => []  
     		|	x::xs  => (get_pattern_type(type_data, x))::helper xs
  	in
		case pattern of
    		Wildcard => Anything
   		|	UnitP => UnitT
		|	ConstP v => IntT
   		|	TupleP v =>  TupleT(helper v)
   		|	Variable v1 =>Anything
   		|	ConstructorP(s,v)=>check_type_data (type_data, s, get_pattern_type(type_data,v)) 
	end


fun get_most_lenient(typ1 ,typ2) =
    let      
        fun helper typelst =
            case  typelst of
                ([],[]) => []  
            |	(x::xs,y::ys)  => (get_most_lenient  (x,y))::helper(xs,ys)
            |	_ => raise NoAnswer
    in
		case (typ1,typ2) of
            (Anything,_)=>typ2
        |	(Datatype s1,Datatype s2) => if s1 = s2 then typ1 else raise NoAnswer
        |	(IntT,IntT)=>IntT
        |	(UnitT,UnitT)=>UnitT
        |	(TupleT v1,TupleT v2) => TupleT(helper(v1,v2))
        |	(_, Anything) =>typ1
        |	(_,_)=> raise NoAnswer
    end


fun typecheck_patterns (type_data, pattern_list) =
    SOME (List.foldl (fn(x,acc) => get_most_lenient ( get_pattern_type(type_data, x),acc) ) Anything pattern_list)
    handle NoAnswer => NONE