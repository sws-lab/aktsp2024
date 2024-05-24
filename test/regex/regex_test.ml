open OUnit2

let tests =
  "regex" >::: [
    Re_parser_test.tests;
    Brzozowski_test.tests;
    Simplify_test.tests;
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
