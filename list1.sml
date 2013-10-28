(* zajecia 13.10.2013 *)

use "base.sml";

(* Zad 1 *)

fun sublist (x::xs) =
    let
        fun addx (ys::yss) = (x::ys) :: addx yss
            | addx nil = nil
        val xss = sublist xs
    in
        xss @ addx xss
    end
    | sublist [] = [[]];

fun sublist' zs = 
    let 
        fun nonEmptySublist (x::xs) = [x] :: foldr (fn (ys, r) => ys :: (x :: ys) :: r) [] (nonEmptySublist xs)
          | nonEmptySublist [] = []
    in 
        [] :: nonEmptySublist zs
    end;

sublist' [1,2,3];

datatype tree = & of int * (tree * tree) | %

infix &

fun flatten (x&(t1,t2)) = flatten t1 @ [x] @ flatten t2
    | flatten % = nil

fun treeFoldr f z % = z
  | treeFoldr f z (x & (t1, t2)) = treeFoldr f (f x (treeFoldr f z t1)) t2;

fun treeFold f z % = z
  | treeFold f z (x & (t1, t2)) = treeFold f (f (x, treeFold f z t2)) t1;

fun flatten' t = treeFold (op ::) [] t;

flatten' (1 & (%, %));
flatten' (2 & (1 & (%,%), 3 & (%,%)));
flatten' (4 & (
    2 & 
        (1 & (%,%), 3 & (%,%)), 
    6 & 
        (5 & (%,%),%)));

fun rev (x::xs) = rev xs @ [x]
    | rev nil = nil

fun rev' xs = foldl (op ::) [] xs;

(*Zad 5 *)

fun selectSort [] = []
  | selectSort (ls as x::xs) = 
        let
            val max = foldr (fn (y, c) => if y > c then y else c) x ls
            val min = foldr (fn (y, c) => if y < c then y else c) x ls
            fun minBounded b r (z::zs) = if z > b andalso z < r then minBounded b z zs else minBounded b r zs
              | minBounded b r [] = r
        in
            rev \> foldr (fn (_, (sorted as (currentMin::_))) => (minBounded currentMin max ls) :: sorted) [min] xs
        end;

selectSort [203, 3, 5, 12, 43, 59, 23];

(* Zad 9 *)

fun suffixes (ys as x::xs) =
        ys :: suffixes xs
  | suffixes [] = [[]];