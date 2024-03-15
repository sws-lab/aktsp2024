open OUnit2
open Toylangs.Bool

let env_ = CharSet.empty
let envA = CharSet.singleton 'A'
let envB = CharSet.singleton 'B'
let envAB = CharSet.of_list ['A'; 'B']

let assert_equal = assert_equal ~printer:string_of_bool

let test_const _ =
  assert_equal false (eval env_ (Var 'A'));
  assert_equal true (eval envA (Var 'A'));
  assert_equal false (eval envA (Var 'B'))

let test_ops _ =
  assert_equal true (eval env_ (Not (Var 'A')));
  assert_equal false (eval envA (Not (Var 'A')));

  assert_equal false (eval env_ (Or (Var 'A', Var 'B')));
  assert_equal true (eval envA (Or (Var 'A', Var 'B')));
  assert_equal true (eval envB (Or (Var 'A', Var 'B')));
  assert_equal true (eval envAB (Or (Var 'A', Var 'B')));

  assert_equal true (eval env_ (Imp (Var 'A', Var 'B')));
  assert_equal false (eval envA (Imp (Var 'A', Var 'B')));
  assert_equal true (eval envB (Imp (Var 'A', Var 'B')));
  assert_equal true (eval envAB (Imp (Var 'A', Var 'B')))

let tests =
  "bool" >::: [
    "const" >:: test_const;
    "ops" >:: test_ops;
  ]
