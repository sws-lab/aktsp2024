open OUnit2

let tests =
  "toylangs" >::: [
    Expr_test.tests;
    Bool_test.tests;
    Rnd_test.tests;
    Imp_test.tests;
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
