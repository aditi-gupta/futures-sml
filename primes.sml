val size = 100000
val grain = 100

structure Future = FutureSuspend

datatype stream'
  = Prime of stream
  | Composite of stream
  | End
and stream = Val of stream' | Fut of (stream' Future.t)

val to_val = fn
  Val t => t
| Fut f => Future.touch f

val rec filter : (int -> int -> stream -> stream') = fn c => fn d => fn s =>
case to_val s of
  Prime t =>
    if c > 0 then Prime (Val (filter (c-1) (d+1) t))
    else Composite (Val (filter d 0 t))
| Composite t => Composite (Val (
    if c > 0 then filter (c-1) (d+1) t
    else filter d 0 t
  ))
| End => End

val rec head : (int -> stream -> stream) = fn x => fn s =>
case to_val s of
  Prime t => Val (Prime (
    head (x + 1) (
      if x mod grain = 0
      then Fut (Future.future (fn () => filter x 0 t))
      else Val (filter x 0 t)
    )
  ))
| Composite t => Val (Composite (
    head (x + 1) t
  ))
| End => Val End

val rec candidates = fn x =>
  if x > 0 then Val (Prime (candidates (x-1)))
  else Val End

val primes = fn x => head 1 (candidates x)

val t0 = Time.now ()
val produced = primes size
val t1 = Time.now ()

val _ = print ("Time:   " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
