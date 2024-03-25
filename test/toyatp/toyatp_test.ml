open OUnit2

let tests =
  "toyatp" >::: [
    Sat_test.tests;
    Smt_test.tests;
    Magic_test.tests;
    Bool_test.tests;
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
