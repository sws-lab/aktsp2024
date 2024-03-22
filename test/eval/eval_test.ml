open OUnit2

let tests =
  "eval" >::: [
    Concrete_test.tests;
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
