open IntInf

val [SIZE,GRAIN] =
  List.map (Option.valOf o IntInf.fromString) (CommandLine.arguments ())

structure Future = FutureSuspend

datatype stream'
  = Prime of stream
  | Composite of stream
  | End
and stream = Val of stream' | Fut of stream' Future.t

val to_val = fn
  Val t => t
| Fut f => Future.touch f

val rec to_string = fn s =>
  case to_val s of
    Prime s' => "Prime (" ^ to_string s' ^ ")"
  | Composite s' => "Comp (" ^ to_string s' ^ ")"
  | End => "End"

(* artificially inflate the work of each future *)
(* val inflate = fn () => OS.Process.sleep SLEEP_TIME *)

val rec filter : (int -> int -> stream -> int -> stream') = fn c => fn d => fn s => fn g =>
  let
    val par = g mod GRAIN = 0
  in
    case to_val s of
      Prime t =>
        if c > 0 then Prime (
          if par then Fut (Future.future (fn () => filter (c-1) (d+1) t (g+1)))
          else Val (filter (c-1) (d+1) t (g+1))
        )
        else Composite (
          if par then Fut ((Future.future (fn () => filter d 0 t (g+1))))
          else Val (filter d 0 t (g+1))
        )
    | Composite t =>
        if c > 0 then Composite (
          if par then Fut (Future.future (fn () => filter (c-1) (d+1) t (g+1)))
          else Val (filter (c-1) (d+1) t (g+1))
        )
        else Composite (
          if par then Fut ((Future.future (fn () => filter d 0 t (g+1))))
          else Val (filter d 0 t (g+1))
        )
    | End => End
  end

(* x is the current number in the stream to consider,
 * k is the length of the stream (k * k is the potential needed for this function),
 * and s is the stream itself *)
val rec head : (int -> int -> stream -> stream) = fn x => fn g => fn s =>
  case to_val s of
    Prime t => Val (Prime (
      head (x+1) (g+1) (
        if g mod GRAIN = 0
        then Fut (Future.future (fn () => filter x 0 t 0))
        else Val (filter x 0 t 0)
      )
    ))
  | Composite t => Val (Composite (
      head (x+1) (g+1) t
     ))
  | End => Val End

val rec candidates = fn x =>
  if x > 0 then Val (Prime (candidates (x-1)))
  else Val End

val primes = head 1 0 o candidates

val t0 = Time.now ()
val result = primes SIZE
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ "\n")
