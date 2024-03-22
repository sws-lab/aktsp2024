open OUnit2
open Toylangs.Imp

let expr1 = Div (Add (Num 5, Add (Num 3, Neg (Num 2))), Num 2)
let expr2 = Add (Add (Num 5, Div (Num 3, Neg (Num 2))), Num 2)
let expr3 = Div (Div (Num 8, Num 2), Num 2)
let expr4 = Div (Num 8, Div (Num 2, Num 2))
let expr5 = Add (Neg expr4, Add (expr2, expr3))

let assert_equal = assert_equal ~printer:string_of_int

let test_no_assign _ =
  assert_equal 10 (eval_prog ([], (Num 10)));
  assert_equal 4 (eval_prog ([], (Add (Num 2, Num 2))));
  assert_equal 3 (eval_prog ([], expr1));
  assert_equal 6 (eval_prog ([], expr2));
  assert_equal 2 (eval_prog ([], expr3));
  assert_equal 8 (eval_prog ([], expr4));
  assert_equal 0 (eval_prog ([], expr5))

let test_single_assign _ =
  assert_equal 5 (eval_prog ([('x', Num 5)], Var 'x'));
  assert_equal 3 (eval_prog ([('x', expr1)], Var 'x'));
  assert_equal (3 + 6) (eval_prog ([('y', expr1)], Add (Var 'y', expr2)))

let test_multiple_assign _ =
  assert_equal (3 + 6) (eval_prog ([('y', expr1); ('z', expr2)], Add (Var 'y', Var 'z')));
  assert_equal (5 + 1) (eval_prog ([('x', Num 5); ('y', Add (Var 'x', Num 1))], Var 'y'))

let test_reassign _ =
  assert_equal 7 (eval_prog ([('x', Num 5); ('y', Num 6); ('x', Num 7)], Var 'x'));
  assert_equal (5 + 1 + 1) (eval_prog ([('x', Num 5); ('y', Add (Var 'x', Num 1)); ('x', Add (Var 'y', Num 1))], Var 'x'))

let test_undefined _ =
  OUnitTodo.assert_raises Not_found (fun () -> eval_prog ([('x', Num 5)], Var 'y'))

let tests =
  "imp" >::: [
    "no_assign" >:: test_no_assign;
    "single_assign" >:: test_single_assign;
    "multiple_assign" >:: test_multiple_assign;
    "reassign" >:: test_reassign;
    "undefined" >:: test_undefined;
  ]
