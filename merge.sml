(* Merging binary trees, from Blelloch and Reid-Miller '97 *) 
(* https://dl.acm.org/doi/pdf/10.1145/258492.258517 *)

val depth1 = 20
val depth2 = 30

val fib_size = 20
val grain = 5

structure Future = FutureSuspend

datatype tree = empty | node of int * tree * tree
type tree' = tree * int

val r = ref 0

fun fib 0 = 1
  | fib 1 = 1
  | fib n = fib (n-1) + fib (n-2)

fun split (s,empty) = Future.future (fn () => (empty,empty))
  | split (s,node(v,L,R)) = 
      if s < v then 
        Future.future (fn () => 
          let val (L1,R1) = Future.touch (split (s,L))
              val () = r := fib fib_size
          in (L1,node(v,R1,R))
          end
        )
      else
        Future.future (fn () =>
          let val (L1,R1) = Future.touch (split (s,R))
              val () = r := fib fib_size
          in (node(v,L,L1),R1)
          end
        )

(* version with no futures *)
fun split' (s,empty) = (empty,empty)
  | split' (s,node(v,L,R)) =
      if s < v then
        let val (L1,R1) = split' (s,L)
            val () = r := fib fib_size
        in (L1,node(v,R1,R))
        end
      else 
        let val (L1,R1) = split' (s,R)
            val () = r := fib fib_size
        in (node(v,L,L1),R1)
        end

(* version with no futures *)
fun merge' (empty,T2) = T2
  | merge' (T1,empty) = T1
  | merge' (node(v,L,R),T2) = 
      let val (L2,R2) = split' (v,T2)
          val left    = merge' (L,L2)
          val right   = merge' (R,R2)
      in node(v,left,right)
      end

fun merge (empty,T2) d = T2
  | merge (T1,empty) d = T1
  | merge (T1 as node(v,L,R),T2) d =
      if d <= grain then merge' (T1,T2) else
      let val (L2,R2) = Future.touch (split (v,T2))
          val left    = Future.future (fn () => merge (L,L2) (d-1))
          val right   = Future.future (fn () => merge (R,R2) (d-1))
      in node(v,Future.touch left,Future.touch right)
      end

(* construct a binary tree of depth d *)
local
  fun build' 0 _ _ = empty
    | build' depth root diff = 
        let val L = build' (depth - 1) (root - diff) (diff div 2)
            val R = build' (depth - 1) (root + diff) (diff div 2)
        in node (root,L,R)
        end
in
  fun build d = build' d (d * 2) d
end

val t1 = build depth1
val t2 = build depth2

val t0 = Time.now ()
val result = merge (t1,t2) (Int.max (depth1,depth2))
val t1 = Time.now ()

val _ = print (LargeInt.toString (Time.toMilliseconds (Time.- (t1, t0))) ^ " ms\n")
