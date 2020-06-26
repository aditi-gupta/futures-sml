open IntInf

val [SIZE,GRAIN,FIB_SIZE] =
  List.map (Option.valOf o IntInf.fromString) (CommandLine.arguments ())
val HALF = SIZE div 2

structure Future = FutureSuspend

val r = ref (true,0,0)
val fst = fn (x,y,z) => x
val snd = fn (x,y,z) => y
val trd = fn (x,y,z) => z

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

datatype prime = prime | composite

datatype stream'
  = null
  | cons of prime * stream
and stream = Val of stream' | Fut of stream' Future.t

val to_val = fn
  Val s => s
| Fut f => Future.touch f

val rec filter = fn c => fn d => fn t =>
let val () = if fst (!r) then () else r := (fst (!r), snd (!r), trd (!r) + 1)
in
case t of
  null => null
| cons (prime,s) =>
    if c > 0 then cons (prime,Val (filter (c-1) (d+1) (to_val s)))
    else cons (composite,Val (filter d 0 (to_val s)))
| cons (composite,s) => cons (composite, Val (
    if c > 0 then filter (c-1) (d+1) (to_val s)
    else filter d 0 (to_val s)
  ))
end

val rec head = fn x => fn g => fn t =>
let val () = ()
in
case t of
  null => null
| cons (prime,s) => (cons (prime,
      head (x + 1) (g + 1) (* THIS IS WRONG??? I'm calling head on something with a thing consed on *)
        if x > HALF then to_val s
        else if g mod GRAIN = 0 orelse g < 10 (* andalso x < MAX *)
        then
          let val () = if fst (!r) then r := (false,1,0) else ()
          in Fut (Future.future (fn () => filter x 0 (to_val s)))
          (* on the next call to head, this future is necessarily going to be touched, because
             we call filter on to_val s => say we have a composite # next, then we'll immediately
             touch the future with to_val because we need the head of the stream. if we're filtering
             we also need the head => need a way to get just the head without necessarily finishing
             the whole computation? do we want this to be lazy but with a future? look at stream type.
             need to separate head from rest of computation so you can get out head without touching
             the future (if you touch the future, you'll just go through until you see a nested future,
             but filtering especially is always going to have to expose the whole thing)
           *)
          end
        else Val (filter x 0 (to_val s))
      ))
| cons (composite,s) => cons (composite, Val (head (x + 1) g (to_val s)))
end

val rec candidates = fn x =>
  if x > 0 then cons (prime, Val (candidates (x-1)))
  else null

val primes = head 1 0 o candidates

val t0 = Time.now ()
val result = primes SIZE
val t1 = Time.now ()

val _ = print ("Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
