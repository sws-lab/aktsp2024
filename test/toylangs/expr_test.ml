open OUnit2
open Toylangs.Expr

let assert_equal = assert_equal ~printer:string_of_int

let test_simple _ =
  assert_equal 10 (eval (Num 10));
  assert_equal 4 (eval (Add (Num 2, Num 2)))

let expr1 = Div (Add (Num 5, Add (Num 3, Neg (Num 2))), Num 2)
let expr2 = Add (Add (Num 5, Div (Num 3, Neg (Num 2))), Num 2)
let expr3 = Div (Div (Num 8, Num 2), Num 2)
let expr4 = Div (Num 8, Div (Num 2, Num 2))
let expr5 = Add (Neg expr4, Add (expr2, expr3))

let test_more _ =
  assert_equal 3 (eval expr1);
  assert_equal 6 (eval expr2);
  assert_equal 2 (eval expr3);
  assert_equal 8 (eval expr4);
  assert_equal 0 (eval expr5)

let tests =
  "expr" >::: [
    "simple" >:: test_simple;
    "more" >:: test_more;
  ]
