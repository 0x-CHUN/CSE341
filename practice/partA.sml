fun sum_list(xs : int list) = 
    if null xs
    then 0
    else hd xs + sum_list(tl xs)

fun countdown(x : int) = 
    if x = 0
    then []
    else x::countdown(x-1)

fun append(xs : int list, ys : int list) = 
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)

fun sum_pair_list(xs : (int*int) list) =
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) +sum_pair_list(tl xs)

fun firsts (xs : (int*int) list) =
    if null xs
    then []
    else #1(hd xs) :: firsts(tl xs)

fun seconds (xs : (int*int) list) =
    if null xs
    then []
    else #2(hd xs) :: seconds(tl xs)

fun sum_pair_list2 (xs : (int*int) list) =
    (sum_list (firsts xs)) + (sum_list (seconds xs))

fun silly1 (z : int) =
    let val x = if z > 0 then z else 34
        val y = x+z+9
    in
        if x > y then x*2 else y*y
    end

fun silly2 () =
    let val x = 1
    in
        (let val x = 2 in x+1 end) +
        (let val y = x+2 in y+1 end)
    end

fun count (from : int , to : int) =
    if from = to
    then to::[]
    else from::count(from+1,to)

fun countup_from1(x : int) = 
    let
        fun count (from : int , to : int) =
            if from = to 
            then to::[]
            else from::count(from+1,to)
    in
        count(1,x)
    end

fun max(xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
        let val tl_ans = max(tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end

fun max_2(xs : int list) =
    if null xs
    then NONE
    else
        let val tl_ans = max_2(tl xs)
        in
            if isSome tl_ans
                andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME (hd xs)
        end

fun sort_pair(pr : int*int) = 
    if #1 pr < #2 pr
    then pr
    else (#2 pr,#1 pr)

fun append(xs : int list, ys : int list) =
    if null xs
    then ys
    else hd (xs) :: append (tl (xs), ys)

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
    |   Negate e2 => ~ (eval e2)
    |   Add(e1,e2) => (eval e1) + (eval e2)
    |   Multiply(e1,e2) => (eval e1) * (eval e2)

fun sum_triple triple = 
    case triple of (x, y, z) => x + y +z

fun full_name r =
    case r of {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

exception ListLengthMismatch

fun zip3 lists =
    case lists of
        ([],[],[]) => []
    |   (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
    |   _ => raise ListLengthMismatch

fun unzip3 triples =
    case triples of
        [] => ([],[],[])
    |   (a,b,c)::tl =>  
            let val (l1, l2, l3) = unzip3 tl in 
                (a::l1,b::l2,c::l3)
            end

fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times (f, n-1, x))

fun double x = x + x
fun increment x = x + 1

fun double_n_times (n, x) = n_times (double, n, x)
fun nth_tail (n, x) = n_times (tl, n, x)

fun triple_n_times (n, x) = n_times(let fun trip y = 3*y in trip end, n, x)

fun map (f, xs) = 
    case xs of
        [] => []
    |   x::xs' => (f x) :: (map (f, xs'))

fun filter (f, xs) =
    case xs of
        [] => []
    |   x::xs' => if f x then x::(filter (f, xs')) else filter (f, xs')

fun allShorterThan1 (xs,s) = filter(fn x => String.size x < String.size s, xs)

fun allShorterThan2 (xs,s) =
    let val i = String.size s
    in 
        filter(fn x => String.size x < i, xs) 
    end

fun fold (f, acc, xs) =
    case xs of
        [] => acc
    |   x::xs => fold(f, f(acc, x), xs)

fun compose (f, g) = fn x => f (g x)

fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i

infix |>
fun x |> f = f x

val sorted3 = fn x => fn y => fn z => z >=y andalso y>= x

fun fold f acc xs =
    case xs of
        [] => acc
    | x::xs' => fold f (f(acc,x)) xs'

val sum = fold (fn (x, y) => x+y) 0

fun exists predicate xs = 
    case xs of
        [] => false
    |   x::xs' => predicate x orelse exists predicate xs'

val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f :: (!cbs)

fun onEvent i =
    let fun loop fs =
            case fs of
                [] => ()
            | f::fs' => (f i; loop fs')
    in loop (!cbs) end

val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i = onKeyEvent (
    fn j => if i = j then print ("pressed " ^ Int.toString i) else ())

signature MATHLIB =
sig
    val fact : int -> int
    val half_pi : real
    val doubler : int -> int
end


structure MyMathLib :> MATHLIB =
struct

fun fact x =
    if x = 0
    then 1
    else x * fact(x-1)

val half_pi = Math.pi / (Real.fromInt 2)

fun doubler x = x * 2

end