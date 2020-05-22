(* Mapping a list of int futures to a list of int futures with each incremented by 1 *)
(* From http://www.cs.cmu.edu/afs/cs/academic/class/15210-f15/www/tapp.html#ch:futures *)

val n = 10000
(* significant slowdown as # of processors increases; 
 * 33 ms w/ 1 processor, 43 w/ 2, 50 w/ 3, 56 w/ 4 *)

structure Future = FutureSuspend

val rec mapincr_help = fn D => fn
    [] => D
  | x::xs => mapincr_help (Future.future (fn () => (Future.touch x) + 1) :: D) xs

fun produce 0 = []
  | produce n = Future.future (fn () => n) :: produce (n-1)

val mapincr = mapincr_help []
val original = produce n

val t0 = Time.now ()
val result = mapincr original
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")