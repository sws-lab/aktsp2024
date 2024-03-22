open OUnit2
open Eval.Common
open Eval.Concrete

let test_eval_binary_arithmetic _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 9 (eval_binary 5 Add 4);
  assert_equal 1 (eval_binary 5 Sub 4);
  assert_equal 20 (eval_binary 5 Mul 4);
  assert_equal 5 (eval_binary 20 Div 4);
  assert_equal 4 (eval_binary 18 Div 4);
  assert_equal 2 (eval_binary 18 Mod 4)

let test_eval_binary_comparison _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 0 (eval_binary 5 Eq 4);
  assert_equal 1 (eval_binary 5 Eq 5);
  assert_equal 1 (eval_binary 5 Ne 4);
  assert_equal 0 (eval_binary 5 Ne 5);
  assert_equal 1 (eval_binary 4 Lt 5);
  assert_equal 0 (eval_binary 5 Lt 5);
  assert_equal 0 (eval_binary 5 Lt 4);
  assert_equal 1 (eval_binary 4 Le 5);
  assert_equal 1 (eval_binary 5 Le 5);
  assert_equal 0 (eval_binary 5 Le 4);
  assert_equal 0 (eval_binary 4 Gt 5);
  assert_equal 0 (eval_binary 5 Gt 5);
  assert_equal 1 (eval_binary 5 Gt 4);
  assert_equal 0 (eval_binary 4 Ge 5);
  assert_equal 1 (eval_binary 5 Ge 5);
  assert_equal 1 (eval_binary 5 Ge 4)

let failure_oracle (_, _) = failwith "failure_oracle"

let test_eval_expr_simple _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 4 (eval_expr Env.empty failure_oracle (Num 4));
  assert_equal 4 (eval_expr (Env.singleton "x" 4) failure_oracle (Var "x"));
  assert_equal 9 (eval_expr (Env.singleton "x" 4) failure_oracle (Binary (Var "x", Add, Num 5)));
  assert_equal 0 (eval_expr (Env.singleton "x" 4) failure_oracle (Binary (Var "x", Gt, Num 5)))

let const_oracle c (l, r) =
  assert (l <= c && c <= r);
  c

let min_oracle (l, _) = l
let max_oracle (_, r) = r

let list_oracle start: oracle =
  let state = ref start in
  fun (l, r) ->
    let i = List.hd !state in
    assert (l <= i && i <= r);
    state := List.tl !state;
    i

let test_eval_expr_rand _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 4 (eval_expr Env.empty (const_oracle 4) (Rand (0, 10)));
  assert_equal 0 (eval_expr Env.empty min_oracle (Rand (0, 10)));
  assert_equal 10 (eval_expr Env.empty max_oracle (Rand (0, 10)));
  assert_equal 8 (eval_expr Env.empty (const_oracle 4) (Binary (Rand (0, 10), Add, Rand (0, 10))));
  assert_equal 0 (eval_expr Env.empty min_oracle (Binary (Rand (0, 10), Add, Rand (0, 10))));
  assert_equal 20 (eval_expr Env.empty max_oracle (Binary (Rand (0, 10), Add, Rand (0, 10))));
  assert_equal 12 (eval_expr Env.empty (list_oracle [8; 4]) (Binary (Rand (0, 10), Add, Rand (0, 10))))

let test_eval_expr_order _ =
  let assert_equal = assert_equal ~printer:string_of_int in
  assert_equal 2 (eval_expr Env.empty (list_oracle [8; 4]) (Binary (Rand (0, 10), Div, Rand (0, 10))));
  assert_equal 0 (eval_expr Env.empty (list_oracle [4; 8]) (Binary (Rand (0, 10), Div, Rand (0, 10))))

let test_eval_stmt_nop _ =
  let assert_equal = assert_equal ~cmp:Env.equal ~printer:Env.show in
  assert_equal (Env.singleton "x" 4) (eval_stmt (Env.singleton "x" 4) failure_oracle Nop)

let test_eval_stmt_assign _ =
  let assert_equal = assert_equal ~cmp:Env.equal ~printer:Env.show in
  assert_equal (Env.singleton "x" 5) (eval_stmt (Env.singleton "x" 4) failure_oracle (Assign ("x", Num 5)));
  assert_equal (Env.add "y" 42 (Env.singleton "x" 5)) (eval_stmt (Env.add "y" 42 (Env.singleton "x" 4)) failure_oracle (Assign ("x", Num 5)));
  assert_equal (Env.singleton "x" 5) (eval_stmt Env.empty failure_oracle (Assign ("x", Num 5)))

let test_eval_stmt_seq _ =
  let assert_equal = assert_equal ~cmp:Env.equal ~printer:Env.show in
  assert_equal (Env.add "y" 10 (Env.singleton "x" 5)) (eval_stmt Env.empty failure_oracle (
      Seq (
        Assign ("x", Num 5),
        Assign ("y", Binary (Var "x", Add, Num 5))
      )
    ))

let test_eval_stmt_if _ =
  let assert_equal = assert_equal ~cmp:Env.equal ~printer:Env.show in
  assert_equal (Env.singleton "x" 5) (eval_stmt Env.empty failure_oracle (
      If (Num 1,
        Assign ("x", Num 5),
        Assign ("x", Num 10)
      )
    ));
  assert_equal (Env.singleton "x" 10) (eval_stmt Env.empty failure_oracle (
      If (Num 0,
        Assign ("x", Num 5),
        Assign ("x", Num 10)
      )
    ));
  assert_equal (Env.singleton "x" 10) (eval_stmt Env.empty min_oracle (
      If (Rand (0, 1),
        Assign ("x", Num 5),
        Assign ("x", Num 10)
      )
    ));
  assert_equal (Env.singleton "x" 5) (eval_stmt Env.empty max_oracle (
      If (Rand (0, 1),
        Assign ("x", Num 5),
        Assign ("x", Num 10)
      )
    ))

let test_eval_stmt_while _ =
  let assert_equal = assert_equal ~cmp:Env.equal ~printer:Env.show in
  assert_equal (Env.singleton "x" 0) (eval_stmt (Env.singleton "x" 0) failure_oracle (
      While (Num 0,
        Assign ("x", Binary (Var "x", Add, Num 1))
      )
    ));
  assert_equal (Env.singleton "x" 10) (eval_stmt (Env.singleton "x" 0) failure_oracle (
      While (Binary (Var "x", Lt, Num 10),
        Assign ("x", Binary (Var "x", Add, Num 1))
      )
    ));
  assert_equal (Env.singleton "x" 0) (eval_stmt (Env.singleton "x" 0) (list_oracle [0]) (
      While (Rand (0, 1),
        Assign ("x", Binary (Var "x", Add, Num 1))
      )
    ));
  assert_equal (Env.singleton "x" 3) (eval_stmt (Env.singleton "x" 0) (list_oracle [1; 1; 1; 0]) (
      While (Rand (0, 1),
        Assign ("x", Binary (Var "x", Add, Num 1))
      )
    ))

let test_eval_stmt_error _ =
  OUnitTodo.assert_raises (Failure "eval_stmt: Error") (fun () -> eval_stmt Env.empty failure_oracle Error)

let tests =
  "concrete" >::: [
    "eval_binary" >::: [
      "arithmetic" >:: test_eval_binary_arithmetic;
      "comparison" >:: test_eval_binary_comparison;
    ];
    "eval_expr" >::: [
      "simple" >:: test_eval_expr_simple;
      "rand" >:: test_eval_expr_rand;
      "order" >:: test_eval_expr_order;
    ];
    "eval_stmt" >::: [
      "nop" >:: test_eval_stmt_nop;
      "assign" >:: test_eval_stmt_assign;
      "seq" >:: test_eval_stmt_seq;
      "if" >:: test_eval_stmt_if;
      "while" >:: test_eval_stmt_while;
      "error" >:: test_eval_stmt_error;
    ];
  ]
