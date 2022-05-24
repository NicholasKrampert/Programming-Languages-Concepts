
datatype exp = 
    Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of  exp * exp
  | Var of string
  | Let of string * exp * exp
  | App of exp * exp
  | Fn of string * exp;

datatype value = 
    CVal of int
  | FnVal of string * exp * (string * value) list
  | Error of string;

(* 
 * 1. implement 
 *      toString: exp -> string 
 *)
fun toString (Int x) = Int.toString x
  | toString (Plus(x, y)) = 
	"(" ^toString(x)^ " + " ^toString(y)^ ")"
  | toString (Minus(x,y)) = 
	"(" ^toString(x)^ " - " ^toString(y)^ ")"
  | toString (Times(x,y)) = 
	"(" ^toString(x)^ " * " ^toString(y)^ ")"
  | toString (Div(x,y)) = 
	"(" ^toString(x)^ " / " ^toString(y)^ ")"
  | toString (Var x) = x
  | toString (Let(x,e1,e2)) = 
	"let val " ^ x ^ "=" ^ toString(e1) ^ " in " ^ toString(e2) ^ " end"
  | toString (App(e1,e2)) = 
	"(" ^toString(e1)^" "^toString(e2)^ ")"
  | toString (Fn(x,e)) = 
	"(fn " ^ x ^ " => " ^ toString(e)^ ")";

(* 2. implement 
 *       toStringValue: value -> string 
 *)
fun toStringValue (CVal x) = Int.toString x
  | toStringValue (FnVal(x, e, _)) = 
	"(fn " ^ x ^ " => " ^ toString(e) ^ ")"
  | toStringValue (Error a) = a;  

(* 
 * you may want to implement the helper function 
 *       lookup: (string * value) list -> string -> value 
 *)
fun lookup nil y = Error("Variable " ^ y ^ " is not found")
  | lookup ((x,w)::ctx) y = if x=y then w else lookup ctx y;
(*
 * 3. implement the function 
 *       eval: exp -> (string * value) list -> value
 *
 * you may find 'case expression' useful in your implementation.
 *)
fun eval e ctx =
  case e of
   (Int x) => CVal x
    | (Plus(e1,e2)) =>
     (case (eval e1 ctx, eval e2 ctx) of
        (CVal x, CVal y) => CVal(x + y)
        | (Error m, _) => Error m
        | (_, Error m) => Error m
        | (v, CVal _) => Error("Plus error: " ^ toStringValue v ^ " is not a number")
        | (_, v) => Error("Plus error: " ^ toStringValue v ^ " is not a number")
		)
	| (Minus(e1,e2)) =>
     (case (eval e1 ctx, eval e2 ctx) of
        (CVal x, CVal y) => CVal(x - y)
        | (Error m, _) => Error m
        | (_, Error m) => Error m
        | (v, CVal _) => Error("Minus error: " ^ toStringValue v ^ " is not a number")
        | (_, v) => Error("Minus error: " ^ toStringValue v ^ " is not a number")
        )
    | (Times(e1,e2)) =>
     (case (eval e1 ctx, eval e2 ctx) of
        (CVal x, CVal y) => CVal(x * y)
        | (Error m, _) => Error m
        | (_, Error m) => Error m
        | (v, CVal _) => Error("Times error: " ^ toStringValue v ^ " is not a number")
        | (_, v) => Error("Times error: " ^ toStringValue v ^ " is not a number")
        )
    | (Div(e1,e2)) =>
     (case (eval e1 ctx, eval e2 ctx) of
        (_, CVal 0) => Error ("Division by zero error: " ^ toString(e))
        | (Error m, _) => Error m
        | (_, Error m) => Error m
        | (CVal x, CVal y) => CVal(x div y)
        | (v, CVal _) => Error("Division error: " ^ toStringValue v ^ " is not a number")
        | (_, v) => Error("Division error: " ^ toStringValue v ^ " is not a number")
        )
    | (Var x) => lookup ctx x
    | (Let(x,e1,e2)) =>
      (case eval e1 ctx of
          Error m => Error m
          | v => eval e2 ((x, v)::ctx))
    | (Fn(x, e)) => FnVal(x, e, ctx)
    | (App(e1, e2)) => (case (eval e1 ctx, eval e2 ctx) of
        (FnVal(x, e, ctx1), v) => eval e ((x, v) :: ctx1)
         | (Error m, _) => Error m
         | (_, Error m) => Error m
         | (v, _) => Error("Application error: " ^ toStringValue(v) ^ " is not a function"))
         ;

(* Test Code *)

Control.Print.printDepth := 100;
Control.Print.stringDepth := 200;

val t1 = Let("y", Int 10, 
                  Let("f", Fn("x", Plus(Var "x", Var "y")), 
                           Let("y", Int 20,
                                    App(Var "f", Int 5))));

toStringValue(eval t1 []);

val t2 = Let("y", Int 10, 
                  Let("f", Fn("x", Plus(Var "x", Var "z")), 
                           Let("y", Int 20,
                                    App(Var "f", Int 5))));
toStringValue(eval t2 []);
                                   

val t3 = Div(Int 10, Int 0);
toStringValue(eval t3 []);

val t4 = Plus(Int 10, Minus (Int 20, Fn("x", Int 3)));
toStringValue(eval t4 []);

val t5 = App(Int 10, Int 20);
toStringValue(eval t5 []);

val t6 = Let("f", Int 10, App(Var "f", Int 20));
toStringValue(eval t6 []);

val t7 = Let("x", Plus(Int 10, Fn("x", Var "x")), Plus(Var "x", Int 20));
toStringValue(eval t7 []);

