(* 
** Rafał Łukaszewski
*)

(*
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
 
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
*)

infix  3 $     fun f $ y = f y;

fun until p f x =
    if p x then x else until p f (f x)