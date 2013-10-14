fun scanl f q ls =  q :: (case ls of
                              []   => []
                            | x::xs => scanl f (f (x,q)) xs);

scanl (fn (x, acc) => x :: acc) [] [1,2,3];

(* Exercise 2.1 *)

fun   suffixes (ys as x::xs) =
        ys :: suffixes xs
    | suffixes [] = [[]];
