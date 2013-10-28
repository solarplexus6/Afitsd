
(** Rafał Łukaszewski
** zajecia 21.10.2013 
*)

use ".\\base.sml";
use "list2.sml";
use "Benchmark.sml";

(*zad 1*)

functor ExplicitMin (H : HEAP) : HEAP =
struct
    structure Elem = H.Elem
    datatype Heap = E | NE of Elem.T * H.Heap

    val empty = E
    (*val isEmpty   : Heap -> bool*)
    fun isEmpty E = true | isEmpty _ = false

    (*val insert    : Elem.T * Heap -> Heap*)
    fun insert (e, E) = NE (e, H.insert (e, H.empty))
      | insert (e, NE (min, heap)) = NE (if Elem.leq (e,min) then e else min, H.insert (e, heap) )
        
    (*val merge     : Heap * Heap -> Heap*)
    fun merge (E, h) = h
      | merge (h, E) = h
      | merge (NE (min1, h1), NE (min2, h2)) = NE (if Elem.leq (min1, min2) then min2 else min1, H.merge (h1,h2))

    (*val findMin   : Heap -> Elem.T*)
    fun findMin E = raise Empty
      | findMin (NE (min, _)) = min

    (*val deleteMin : Heap -> Heap*)
    fun deleteMin E = raise Empty
      | deleteMin (NE (_, heap))= let
                                    val newHeap = H.deleteMin heap
                                  in
                                    NE (H.findMin newHeap, newHeap)
                                  end        

  (*val fromList : Elem.T list -> Heap*)
  fun fromList xs = E
end

(*Zad 22*)

datatype Color = R | B
datatype 'a RBTree = E | N of Color * 'a RBTree * 'a * 'a RBTree

datatype 'a Digit = One of 'a * 'a RBTree
                  | Two of 'a * 'a RBTree * 'a * 'a RBTree

(*incr :: Digit a -> [ Digit a ] -> [ Digit a ]*)
fun incr (One (a, t)) [] = [One (a, t)]
  | incr (One (a1, t1)) ((One (a2,t2)) :: ps) = Two (a1, t1, a2, t2) :: ps
  | incr (One (a1, t1)) (Two (a2, t2, a3, t3) :: ps) = One (a1, t1) :: incr (One (a2, N (B, t2, a3, t3))) ps

fun link (One (a, t), l) = N (B, l, a, t)
  | link (Two (a1, t1, a2, t2), l) = N (B, N (R, l, a1, t1), a2, t2)
fun linkAll xs = foldl link E xs
fun add (a, ps) = incr (One (a, E)) ps

(*bottom-up :: [ a ] -> RBTree a*)
fun bottomup xs = linkAll (foldr add [] xs)

fun color c (N (_, l, x, r)) = N (c, l, x, r)
fun fromOrdList xs = let
  fun build xs 0 = (E, 1, xs)
    | build (x::xs) 1 = (N (B, E, x, E), 2, xs)
    (*Size of the red-rooted subtree grows as $2^(2n)$, so no red node has red child*)
    | build xs n = let
        val mid = n div 2
        val (l, lh, (x::xs')) = build xs mid
        val (r, rh, xs'') = build xs' (n - mid - 1)        
    in
      (N (B, (if lh > rh then color R l else l), x, r), rh + 1, xs'')
    end
        
in
  #1 (build xs (length xs))
end
    

(*foldr add [] [1,2,3, 4, 5];*)

(*https://github.com/kgeorgiy/okasaki/blob/master/Okasaki/Chapter3/RBTree.hs*)

(*Zad 19*)

val s = Random.rand (3459234, 123345)

fun randNatList 0 = []
  | randNatList n = Random.randNat s :: randNatList (n - 1);

val n = 1000000;
val randList1 = randNatList n;
val randList2 = randNatList n;
val randList3 = randNatList n;
val randList4 = randNatList n;

print "*** fromList ***\n";
Benchmark.start();
val benchLHeap = lHeap.fromList randList1;
Benchmark.stopAndPrint "Leftist heap - fromList";

Benchmark.start();
val benchBHeap = bHeap.fromList randList1;
Benchmark.stopAndPrint "Binomial heap - fromList";

print "*** insert ***\n";
Benchmark.start();
val _ = foldl (fn (k, heap) => lHeap.insert (k,heap)) lHeap.empty randList2;
Benchmark.stopAndPrint "Leftist heap - insert";

Benchmark.start();
val _ = foldl (fn (k, heap) => bHeap.insert (k,heap)) bHeap.empty randList2;
Benchmark.stopAndPrint "Binomial heap - insert";

print "*** deleteMin ***\n";
Benchmark.start();
val _ = foldl (fn (k, heap) => lHeap.deleteMin heap) benchLHeap randList1;
Benchmark.stopAndPrint "Leftist heap - deleteMin";

Benchmark.start();
val _ = foldl (fn (k, heap) => bHeap.deleteMin heap) benchBHeap randList1;
Benchmark.stopAndPrint "Binomial heap - deleteMin";

(*
*** fromList ***

Leftist heap - fromList
  User: 2.463, System: 0.000, Total: 2.463
  User: 2.986, System: 0.000, Total: 2.986
  User: 2.987, System: 0.000, Total: 2.987
  User: 11.696, System: 0.000, Total: 11.696

Binomial heap - fromList
  User: 3.150, System: 0.000, Total: 3.150
  User: 4.349, System: 0.000, Total: 4.349
  User: 4.342, System: 0.000, Total: 4.342
  User: 42.323, System: 0.000, Total: 42.323

*** insert ***

Leftist heap - insert
  User: 1.168, System: 0.000, Total: 1.168
  User: 2.086, System: 0.000, Total: 2.086
  User: 1.695, System: 0.000, Total: 1.695
  User: 164.797, System: 0.000, Total: 164.797

Binomial heap - insert
  User: 0.861, System: 0.000, Total: 0.861
  User: 0.946, System: 0.000, Total: 0.946
  User: 0.546, System: 0.000, Total: 0.546
  User: 19.513, System: 0.000, Total: 19.513

*** deleteMin ***

Leftist heap - deleteMin
  User: 3.189, System: 0.000, Total: 3.189
  User: 3.310, System: 0.000, Total: 3.310
  User: 3.802, System: 0.000, Total: 3.802
  User: 174.718, System: 0.000, Total: 174.718

Binomial heap - deleteMin
  User: 4.195, System: 0.000, Total: 4.195
  User: 4.493, System: 0.000, Total: 4.493
  User: 3.808, System: 0.000, Total: 3.808
  User: 229.663, System: 0.000, Total: 229.663
*)