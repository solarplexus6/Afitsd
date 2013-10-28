
(*
** Rafał Łukaszewski
** zajecia 21.10.2013 
*)

use "base.sml";

datatype tree = & of int * (tree * tree) | %

infix &

fun treeFold f z % = z
  | treeFold f z (x & (t1, t2)) = treeFold f (f (x, treeFold f z t2)) t1;

fun flatten' t = treeFold (op ::) [] t;

(* zad 11 *)

(*bez ujemnych n*)
(*cbt: int -> int -> tree*)
fun cbt 0 label = label & (%, %)
  | cbt n label = let 
                    val subtree = cbt (n - 1) label 
                  in
                     label & (subtree, subtree)
                  end;

fun create2 0 label = let
                        val empty = %
                      in
                        (label & (empty, empty), empty)
                      end
  | create2 1 label = let
                        val (t1, t2) = (create2 0 label)
                      in
                        (label & (t1, t2), t1)
                      end
  | create2 n label = let
    val m = (n - 1) div 2
    val (subM1, subM) = create2 m label
  in
    if (n - 1) mod 2 = 0 then (label & (subM1, subM), label & (subM, subM)) else (label & (subM1, subM1), label & (subM1, subM))
  end;

(*bez ujemnych n*)
(*bbt: int -> int -> tree*)
fun bt n label = #2 (create2 n label);

(* Source code from
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press
 *)

(***********************************************************************)
(*                              Chapter 3                              *)
(***********************************************************************)

signature ORDERED =
  (* a totally ordered type and its comparison functions *)
sig
  type T
 
  val leq : T * T -> bool
end

signature HEAP =
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T   (* raises Empty if heap is empty *)
  val deleteMin : Heap -> Heap     (* raises Empty if heap is empty *)

  val fromList : Elem.T list -> Heap
end

(* zad 14 *)

functor LeftistHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T (r,_,_,_)) = r
  fun makeT (x, a, b) = if rank a >= rank b then T (rank b + 1, x, a, b)
                        else T (rank a + 1, x, b, a)

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (_, x, a1, b1), h2 as T (_, y, a2, b2)) =
        if Elem.leq (x, y) then makeT (x, a1, merge (b1, h2))
        else makeT (y, a2, merge (h1, b2))

  fun insert (x, h) = merge (T (1, x, E, E), h)

  fun findMin E = raise Empty
    | findMin (T (_, x, a, b)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (_, x, a, b)) = merge (a, b)

  fun fromList [] = empty
    | fromList xs = let
      fun mergePairs (x::x'::xs) = (merge (x, x')) :: mergePairs xs
        | mergePairs [x] = [x]
        | mergePairs [] = []            
    in
      hd (until (fn hs => case hs of [_] => true | _ => false) mergePairs \> map (fn x => T (1, x, E, E)) xs)
    end
end

functor BinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, c)) = r
  fun root (Node (r, x, c)) = x
  fun link (t1 as Node (r, x1, c1), t2 as Node (_, x2, c2)) =
        if Elem.leq (x1, x2) then Node (r+1, x1, t2 :: c1)
        else Node (r+1, x2, t1 :: c2)
  fun insTree (t, []) = [t]
    | insTree (t, ts as t' :: ts') =
        if rank t < rank t' then t :: ts else insTree (link (t, t'), ts')

  fun insert (x, ts) = insTree (Node (0, x, []), ts)
  fun merge (ts1, []) = ts1
    | merge ([], ts2) = ts2
    | merge (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: merge (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: merge (ts1, ts2')
        else insTree (link (t1, t2), merge (ts1', ts2'))

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
  in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end

  (*fun findMin ts = let val (t, _) = removeMinTree ts in root t end*)
  
  (* zad 16 *)
  fun findMin [] = raise Empty
    | findMin [t] = root t
    | findMin ts =
        foldl (fn (t, acc) => if Elem.leq (root t, acc) then root t else acc) (root \> hd ts) ts

  fun deleteMin ts =
        let val (Node (_, x, ts1), ts2) = removeMinTree ts
        in merge (rev ts1, ts2) end

  fun fromList [] = empty
    | fromList xs = let
      fun mergePairs (x::x'::xs) = (merge (x, x')) :: mergePairs xs
        | mergePairs [x] = [x]
        | mergePairs [] = []            
    in
      hd (until (fn hs => case hs of [_] => true | _ => false) mergePairs \> map (fn x => [Node (0, x, [])]) xs)
    end
end

structure Integers : ORDERED =
struct
  type T = int
  fun leq (x, y) =
    x <= y
end

structure lHeap = LeftistHeap (Integers)
structure bHeap = BinomialHeap (Integers)

val testLHeap = lHeap.fromList [23, 12, 56, 54, 21, 5, 87, 78, 44];
val testBHeap = bHeap.fromList [23, 12, 56, 54, 21, 5, 87, 78, 44];