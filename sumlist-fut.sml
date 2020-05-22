(* From Blelloch & Reid-Miller, 1997. https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val size = 1000
val grain = 500

(* RESULTS
 * 4 processors => 3539 microseconds; 1 processor => 6264 microseconds
 * with grain = 0, more processors keeps time mostly constant
 * with grain = 2000, i.e. no futures, 1 processors takes 6150 microseconds and 
 * 4 takes 142 microseconds
 * 
 * with size = 500, grain = 250, very little speedup between 1 (2651 microseconds) 
 * and 4 (2595 microseconds) processors - again mostly constant
 *)

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list = null | cons of int * list Future.t

(* produces a list of decreasing integers *)
fun produce 0 = null
  | produce n = cons (n, Future.future (fn () => produce (n-1)))

(* version with no futures *)
fun produce' 0 = []
  | produce' n = n :: produce' (n-1)

(* version with no futures *)
fun consume' (sum,[]) = sum
  | consume' (sum,x::xs) = consume' (x + sum,xs)

(* "consumes" elements of a list by summing them *)
fun consume (sum,null) = sum
  | consume (sum,cons(x,xs)) = 
      if x <= grain then consume' (sum, produce' x)
      else consume (x + sum, Future.touch xs)

val t0 = Time.now ()
val result = consume (0,produce size)
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")