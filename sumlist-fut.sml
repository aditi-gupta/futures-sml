(* From Blelloch & Reid-Miller, 1997. https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val size = 1000 (* if you go above this you start getting "copying!" and "resizing!" messages *)
val grain = 500

(* RESULTS
 * 1000/500:
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 1 --
   Produce Time: 2842 us
   Consume Time: 65 us
   Total Time:   2907 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 4 --
   Produce Time: 30 us
   Consume Time: 3586 us
   Total Time:   3616 us

 * 1000/~1: (completely futures)
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 1 --
   Produce Time: 5609 us
   Consume Time: 118 us
   Total Time:   5727 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 4 --
   Produce Time: 27 us
    Consume Time: 5210 us
   Total Time:   5237 us
  
 * 1000/2000: (no futures)
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 1 --
   Produce Time: 76 us
   Consume Time: 11 us
   Total Time:   87 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 4 --
   Produce Time: 124 us
   Consume Time: 11 us
   Total Time:   135 us  

 * 1500/500:
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 1 --
   Produce Time: 5985 us
   Consume Time: 135 us
   Total Time:   6120 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 2 --
   Produce Time: 23 us
   Consume Time: 11461 us
   Total Time:   11484 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 3 --
   Produce Time: 29 us
   Consume Time: 7410 us
   Total Time:   7439 us
 * aditig@pbbs:~/examples$ ./sumlist-fut @MLton number-processors 4 --
   Produce Time: 34 us
   Consume Time: 4993 us
   Total Time:   5027 us
 *)

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list' = Fut of list Future.t | Val of list
and list = null | cons of int * list'

(* produces a list of decreasing integers *)
fun produce 0 = null
  | produce n = cons (n, 
      if n <= grain then Val (produce (n-1))
      else Fut (Future.future (fn () => produce (n-1)))
    )

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