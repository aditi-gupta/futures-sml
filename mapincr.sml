val n = 10

structure Future = FutureSuspend

val rec mapincr_help = fn D => fn
    [] => D
  | x::xs => mapincr_help (Future.future (fn () => (Future.touch x) + 1) :: D) xs

fun fact 0 = 1
  | fact n = n * fact (n-1)

fun produce 0 = []
  | produce n = Future.future (fn () => fact n) :: produce (n-1)

val  mapincr = mapincr_help []

val t0 = Time.now ()
val result = mapincr (produce n)
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")