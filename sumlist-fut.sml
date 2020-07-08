(* From Blelloch & Reid-Miller, 1997. https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val SIZE = 50000
val GRAIN = 1000
val POT = SIZE

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list' = Fut of list Future.t | Val of list
and list = null | cons of int * list'

(* produces a list of decreasing integers *)
fun produce_par 0 w = null
  | produce_par n w =
      cons (n,Fut (Future.future (fn () => produce (n-1) w)))

and produce_seq 0 w = null
  | produce_seq n w = cons (n,Val (produce (n-1) w))

and produce n w =
  case w mod GRAIN of
    0 => produce_par n (w-1)
  | _ => produce_seq n (w-1)

(* "consumes" elements of a list by summing them *)
fun consume (sum,null) = sum
  | consume (sum,cons(x,xs)) = (
      case xs of
        Val l => consume (x + sum,l)
      | Fut f => consume (x + sum,Future.touch f)
    )

fun sum n w = consume (0,produce n w)

val t0 = Time.now ()
val result = sum SIZE POT
val t1 = Time.now ()

val _ = print ("Total Time:   " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")

val _ = print (Int.toString result ^ "\n")