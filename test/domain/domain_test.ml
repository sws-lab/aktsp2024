open OUnit2

let tests =
  "domain" >::: [
    IntDomain_test.tests;
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
