(* 
** Rafał Łukaszewski
*)

(*
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
 
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
*)

(*http://stackoverflow.com/questions/14412439/see-sml-full-list*)
(*Control.Print.printDepth := 10;*)

infix  3 \>     fun f \> y = f y;

fun until p f x =
    if p x then x else until p f (f x)