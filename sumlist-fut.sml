val size = 50
(* TODO: val grain *)

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list = null | cons of int * list Future.t

fun produce 0 = null
  | produce n = cons (n, Future.future (fn () => produce (n-1)))

fun consume (sum,null) = sum
  | consume (sum,cons(x,xs)) = consume (x + sum, Future.touch xs)

val t0 = Time.now ()
val result = consume (0,produce size)
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")
