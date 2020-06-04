(* From Blelloch & Reid-Miller, 1997. https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val size = 50000 (* if you go above this you start getting "copying!" and "resizing!" messages *)
val grain = 1000

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list' = Fut of list Future.t | Val of list
and list = null | cons of int * list'

(* produces a list of decreasing integers *)
fun produce 0 = null
  | produce n = cons (n, 
      if n <= grain then Val (produce (n-1))
      else Fut (Future.future (fn () => val_produce (n-1)))
    )
and val_produce n =
  case n mod 100 of 
    0 => produce n
  | _ => cons (n,Val (val_produce (n-1)))

(* "consumes" elements of a list by summing them *)
fun consume (sum,null) = sum
  | consume (sum,cons(x,xs)) = (
      case xs of 
        Val l => consume (x + sum,l)
      | Fut f => consume (x + sum,Future.touch f)
    )

val t0 = Time.now ()
val produced = produce size
val t1 = Time.now ()
val result = consume (0,produced)
val t2 = Time.now ()

val _ = print ("Produce Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
val _ = print ("Consume Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t1))) ^ " us\n")
val _ = print ("Total Time:   " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")