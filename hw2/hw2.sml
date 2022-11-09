(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, xs) =
   case xs of
      [] => NONE
   |  x::xs' => case same_string(s, x) of
                     true => SOME(xs')
                  |  false => case all_except_option(s, xs') of
                                 NONE => NONE
                              |  SOME y => SOME(x::y)

fun get_substitutions1(strLists, s) =
   case strLists of
      [] => []
   |  strList::strLists' => 
      let 
         val check = all_except_option(s, strList)
      in 
         case check of
            NONE => get_substitutions1(strLists', s)
         |  SOME strs => strs @ get_substitutions1(strLists', s)
      end

fun get_substitutions2(strLists, s) =
   let fun helper(xs, s, ans) = 
      case xs of
         [] => ans
      |  x::xs' => case all_except_option(s, x) of
                        NONE => helper(xs', s, ans)
                     |  SOME(res) => helper(xs', s, ans @ res)
   in
      helper(strLists, s, [])
   end


type Name = {first:string, middle:string, last:string}

fun similar_names(nameLists, name) =
   let fun helper(xs, ans) =
      case xs of
         [] => ans
      |  x::xs' => helper(xs', ans @ [{first=x, middle=(#middle name), last=(#last name)}])
   in
      helper(get_substitutions2(nameLists, #first name), [name])
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (suit, _) =
   case suit of
      Spades => Black
   |  Clubs => Black
   |  Diamonds => Red
   |  Hearts => Red

fun card_value (_, rank) =
   case rank of
      Jack => 10
   |  Queen => 10
   |  King => 10
   |  Ace => 11
   |  Num i => i

fun remove_card (cs, c, e) =
   case cs of
      [] => raise e
   |  x::xs => case c = x of
                  true => xs
               |  false => case remove_card(xs, c, e) of
                              [] => [x]
                           |  y::ys => x::y::ys


fun all_same_color(cs) =
   case cs of
      [] => true
   |  x::[] => true
   |  x::y::rest => case card_color(x) = card_color(y) of
                        true => all_same_color(y::rest)
                     |  false => false


fun sum_cards(cs) =
   case cs of
      [] => 0
   |  x::xs => card_value(x) + sum_cards(xs)

fun score (cs, goal) = 
   let fun helper (cs) =
      case (sum_cards(cs), goal) of
            (sum, goal) => case sum > goal of
                           true => (sum - goal) * 3
                        |  false => goal - sum
   in
      case all_same_color(cs) of
         true => helper(cs) div 2
      |  false => helper(cs)
   end

fun officiate(cs, ms, goal) =
   let fun process(cs, ms, held) = 
      case ms of
         [] => held
      |  m::tail => case m of
                     Discard card => process(cs, tail, remove_card(held, card, IllegalMove))
                  |  Draw => case cs of
                                 [] => held
                              |  c::_ => case sum_cards(c::held) > goal of
                                             true => c::held
                                          |  false => process(remove_card(cs, c, IllegalMove), tail, c::held)
   in
      score(process(cs, ms, []), goal)
   end