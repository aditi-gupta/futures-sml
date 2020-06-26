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

datatype stream'
  = Prime of stream
  | Composite of stream
  | End
and stream = Val of stream' | Fut of stream' Future.t

val expose = fn
  Val t => t
| Fut f => Future.touch f

val to_val = fn
  Val t => t
| Fut f => Future.touch f

val rec to_string = fn s =>
  case to_val s of
    Prime s' => "Prime (" ^ to_string s' ^ ")"
  | Composite s' => "Comp (" ^ to_string s' ^ ")"
  | End => "End"

(* this will not work. that's because when we start a filter, in order to get
   any elements out of it, we MUST have finished the whole filtration process.
   so our original model does not work, because in order to achieve parallelism,
   we need to be able to see the first few elements without forcing the entire
   filter to finish (because those are the only ones we need immediately in head;
   the others can wait, because they'll only be used later in head). currently,
   we are required to force the future that starts the filter immediately after
   it's spawned; this is unavoidable, since we need to be able to see the first
   few elements of this filtered stream (we would only be able to do them all in
   parallel as we thought _if_ we did not need any of those elements until the
   very end).

   to avoid this, we must somehow introduce futures within the filter, so that it
   can compute chunks of the stream without forcing all of it to finish. this is
   a similar process to the production in sumlist; we can think of filter as
   corresponding to produce, and head to consume. it is true that we can start
   multiple filtrations in parallel, but _only_ if we allow part of it to finish
   without forcing all of it.

   for rast, this means changing the order of things - currently, the recursive
   calls to filter are tail calls, which means you can't use PSpawn (???) because
   there is nothing to do in parallel within that procedure (can you change this
   so it doesn't have to be within the procedure?)
 *)
val rec filter : (int -> int -> stream -> stream') = fn c => fn d => fn s =>
  let
    val () = r := fib FIB_SIZE
  in
    case to_val s of
      Prime t =>
        if c > 0 then Prime (Val (filter (c-1) (d+1) t))
        else Composite (Val (filter d 0 t))
    | Composite t => Composite (Val (
        if c > 0 then filter (c-1) (d+1) t
        else filter d 0 t
      ))
    | End => End
  end

(* val filter' = let val () = r := fib FIB_SIZE in filter end *)

val rec head : (int -> int -> stream -> stream) = fn x => fn g => fn s =>
  case to_val s of
    Prime t => Val (Prime (
      head (x + 1) (g + 1) (
        if x > HALF then t
        else if g mod GRAIN = 0 orelse g < 10 (* andalso x < MAX *)
        then (print "future\n"; Fut (Future.future (fn () => filter x 0 t)))
        else Val (filter x 0 t)
      )
    ))
  | Composite t => Val (Composite (
      head (x + 1) g t
     ))
  | End => Val End

val rec candidates = fn x =>
  if x > 0 then Val (Prime (candidates (x-1)))
  else Val End

val primes = head 1 0 o candidates

val c = candidates SIZE
val t0 = Time.now ()
val result = head 1 0 c
val t1 = Time.now ()

(* val _ = print ("Candidates Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
val _ = print ("Head Time:       " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t1))) ^ " us\n")
val _ = print ("Total Time:      " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t0))) ^ " us\n") *)

(* val _ = print ((to_string result) ^ "\n") *)

val _ = print ("Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
