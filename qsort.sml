(* From Blelloch & Reid-Miller, 1997. https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val [SIZE,FIB_SIZE] =
  List.map (Option.valOf o Int.fromString) (CommandLine.arguments ())

(* implemented by schedulers/spoonhower *)
structure Future = FutureSuspend

datatype list = null | cons of int * list'
and list' = Fut of list Future.t | Val of list

val r = ref 1
fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

fun to_val (Val v) = v
  | to_val (Fut f) = Future.touch f

fun partition (elt,null) = Future.future (fn () => (null,null))
  | partition (elt,cons (h,t)) =
      Future.future (fn () =>
        let val (l,g) = Future.touch (partition' (elt,to_val t))
        in if elt > h then (cons (h,Val l),g) else (l,cons (h,Val g))
        end
      )

and partition' x = let val () = r := fib FIB_SIZE
                   in partition x end

fun qs (null,rest) = rest
  | qs (cons (h,t),rest) =
    let val (l,g) = Future.touch (partition' (h,to_val t))
    in qs (l,cons (h,Fut (Future.future (fn () => qs (g,rest)))))
    end

fun qsort l = qs (l,null)

local
  val seed = Random.rand (123,456)
  fun loop 0 = null
    | loop n = cons (Random.randRange (1,100) seed,Val (loop (n-1)))
in
  val l = loop 100
end

val t0 = Time.now ()
val result = qsort l
val t1 = Time.now ()

val _ = print ("Total Time:   " ^ LargeInt.toString (Time.toMicroseconds (Time.- (t1, t0))) ^ " us\n")