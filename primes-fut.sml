open IntInf

val [SIZE,GRAIN,FIB_SIZE] =
  List.map (Option.valOf o IntInf.fromString) (CommandLine.arguments ())
val HALF = SIZE div 2
val POT = SIZE * SIZE

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

val to_val = fn
  Val t => t
| Fut f => Future.touch f

val rec to_string = fn s =>
  case to_val s of
    Prime s' => "Prime (" ^ to_string s' ^ ")"
  | Composite s' => "Comp (" ^ to_string s' ^ ")"
  | End => "End"

val rec filter : (int -> int -> stream -> int -> stream') = fn c => fn d => fn s => fn w =>
  let
    val () = r := fib FIB_SIZE
    val par = w mod GRAIN = 0
  in
    case to_val s of
      Prime t =>
        if c > 0 then Prime (
          if par then Fut (Future.future (fn () => filter (c-1) (d+1) t (w-1)))
          else Val (filter (c-1) (d+1) t (w-1))
        )
        else Composite (
          if par then Fut ((Future.future (fn () => filter d 0 t (w-1))))
          else Val (filter d 0 t (w-1))
        )
    | Composite t =>
        if c > 0 then Composite (
          if par then Fut (Future.future (fn () => filter (c-1) (d+1) t (w-1)))
          else Val (filter (c-1) (d+1) t (w-1))
        )
        else Composite (
          if par then Fut ((Future.future (fn () => filter d 0 t (w-1))))
          else Val (filter d 0 t (w-1))
        )
    | End => End
  end

(* x is the current number in the stream to consider,
 * k is the length of the stream (k * k is the potential needed for this function),
 * and s is the stream itself *)
val rec head : (int -> int -> stream -> stream) = fn x => fn k => fn s =>
  case to_val s of
    Prime t => Val (Prime (
      head (x+1) (k-1) (
        if x > HALF then t
        else if (k * k) mod GRAIN = 0
        then Fut (Future.future (fn () => filter x 0 t (k-1)))
        else Val (filter x 0 t (k-1))
      )
    ))
  | Composite t => Val (Composite (
      head (x+1) (k-1) t
     ))
  | End => Val End

val rec candidates = fn x =>
  if x > 0 then Val (Prime (candidates (x-1)))
  else Val End

val primes = fn w => head 1 w o candidates

val t0 = Time.now ()
val result = primes POT SIZE
val t1 = Time.now ()

val _ = print ("Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
