val [SIZE,GRAIN] =
  List.map (Option.valOf o Int.fromString) (CommandLine.arguments ())
val HALF = SIZE div 2
(* val MAX = HALF - HALF div RATIO *)

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

val rec head : (int -> int -> stream -> stream) = fn x => fn g => fn s =>
  case to_val s of
    Prime t => Val (Prime (
      head (x + 1) (g + 1) (
        if x > HALF then t
        else if g mod GRAIN = 0 orelse g < 10 (* andalso x < MAX *)
        then (print (Int.toString (x + 1) ^ " fut\n"); Fut (Future.future (fn () => filter x 0 t)))
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

val primes = fn x => head 1 0 (candidates x)

val t0 = Time.now ()
val result = primes SIZE
val t1 = Time.now ()

(* val _ = print ("Candidates Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
val _ = print ("Head Time:       " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t1))) ^ " us\n")
val _ = print ("Total Time:      " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t2, t0))) ^ " us\n") *)

(* val _ = print ((to_string result) ^ "\n") *)

val _ = print ("Time: " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")
