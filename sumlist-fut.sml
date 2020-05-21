val size = 1000
val grain = 500
(* works better with 4 processors than 1, but is significantly 
 * slower (3.6 ms) than with no futures (.14 ms) *)

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list = null | cons of int * list Future.t

fun produce 0 = null
  | produce n = cons (n, Future.future (fn () => produce (n-1)))

fun produce' 0 = []
  | produce' n = n :: produce' (n-1)

fun consume' (sum,[]) = sum
  | consume' (sum,x::xs) = consume' (x + sum,xs)

fun consume (sum,null) = sum
  | consume (sum,cons(x,xs)) = 
      if x <= grain then consume' (sum, produce' x) 
      else consume (x + sum, Future.touch xs)

val t0 = Time.now ()
val result = consume (0,produce size)
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")