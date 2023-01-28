datatype int_or_string = Int of int | Str of string ;

(*we are making our own list using datatype*)

datatype int_or_string list = Empty | Cons of int_or_string * int_or_string list ;


(*here is an example of int or string  --->

((Cons Str "hello" (Cons (Int 3 , Empty)))

*)



fun sum2 (x:int_or_string list ) =
 case x of
    Empty => 0
    | Cons (Int i, y) => i + sum2 (y)
    | Cons (Str s, y) => string.size s + sum2 y;


val a = Cons ((Str "hi"), (Cons (Int 3, Empty)))

sum2 a; 
