datatype exp = Const of int | Add of exp * exp | Negate of exp | Mult of exp * exp ;


(*tree like structure to perform arthimatic operations *)

(* exp -> int*)
fun eval_exp_old  (x : exp ) =
    case x of
	Const i => i
  | Add (e1, e2)  => eval_exp_old e1 + eval_exp_old e2
  | Negate e1  =>  ~ (eval_exp_old e1)
  | Mult (e1,e2)   => ( eval_exp_old e1) * (eval_exp_old e2);

val a = Mult ((Const 4) , (Const 4));

exception Error of string;

eval_exp_old a ;

fun get_int e =
    case e of
	Const i => i
      | _  => raise ( Error "ony constructor made by int can be eextracted")
;

(*exp -> exp *)
fun eval_exp_new e =
      case e of
	  Const i => e
	| Negate e1 => Const (~ (get_int (eval_exp_new e1)))
	| Add (e1, e2) =>(Const (get_int  (eval_exp_new e1) + get_int (eval_exp_new e2)))
	| Mult (e1, e2)  => (Const (get_int  (eval_exp_new e1) * get_int (eval_exp_new e2)));



eval_exp_new a;


