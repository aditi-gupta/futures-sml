val size = 10
(* TODO: val grain *)

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

fun fact 0 = 1
  | fact n = n * fact (n-1)

fun produce 0 = []
  | produce n = Future.future (fn () => fact n) :: produce (n-1)

fun consume (sum,[]) = sum 
  | consume (sum,x::xs) = consume (Future.touch x + sum,xs)

val t0 = Time.now ()
val result = consume (0,produce size)
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")
