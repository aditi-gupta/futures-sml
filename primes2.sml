open IntInf

val [SIZE,GRAIN,FIB_SIZE] =
  List.map (Option.valOf o IntInf.fromString) (CommandLine.arguments ())
val HALF = SIZE div 2
(* val MAX = HALF - HALF div RATIO *)

structure Future = FutureSuspend

val r = ref 1

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

datatype prime = prime | composite

datatype stream
  = null
  | cons_fut of prime * stream Future.t
  | cons_val of prime * stream

val rec filter = fn c => fn d => fn
  null => null
| cons_fut (prime,f) =>
    if c > 0 then cons_val (prime,filter (c-1) (d+1) (Future.touch f))
    else cons_val (composite,filter d 0 (Future.touch f))
| cons_val (prime,l) =>
    if c > 0 then cons_val (prime,filter (c-1) (d+1) l)
    else cons_val (composite,filter d 0 l)
(* | cons_fut (composite,f) => cons_val (composite,
    if c > 0 then filter (c-1) (d+1) (Future.touch f)
    else filter d 0 (Future.touch f)
  ) *)
| cons_val (composite,l) => cons_val (composite,
    if c > 0 then filter (c-1) (d+1) l
    else filter d 0 l
  )

val rec head = fn x => fn g => fn
  null => null
| cons_fut (prime,f) =>
      head (x + 1) (g + 1) (
        if x > HALF then Future.touch f
        else if g mod GRAIN = 0 orelse g < 10 (* andalso x < MAX *)
        then cons_fut (prime, (Future.future (fn () => filter x 0 (Future.touch f))))
        else cons_val (prime, filter x 0 (Future.touch f))
      )
| cons_val (prime,l) =>
    head (x + 1) (g + 1) (
        if x > HALF then l
        else if g mod GRAIN = 0 orelse g < 10 (* andalso x < MAX *)
        then cons_fut (prime, Future.future (fn () => filter x 0 l))
        else cons_val (prime, filter x 0 l)
    )
| cons_fut (composite,f) => raise Fail "got here" (* cons_val (composite, head (x + 1) g (Future.touch f)) *)
| cons_val (composite,l) => cons_val (composite, head (x + 1) g l)

val rec candidates = fn x =>
  if x > 0 then cons_val (prime, candidates (x-1))
  else null

val primes = head 1 0 o candidates

val t0 = Time.now ()
val result = primes SIZE
val t1 = Time.now ()

val _ = print ("Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
