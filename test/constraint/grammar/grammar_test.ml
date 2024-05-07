open OUnit2
open Constraint_grammar

let test_example_nullable _ =
  let module Solver = Constraint.Solver.Kleene (ExampleGrammar.NullableSys) in
  let sol = Solver.solve () in
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal true (Solver.VH.find sol T);
  assert_equal true (Solver.VH.find sol R)

let test_example_first _ =
  let module Solver = Constraint.Solver.Kleene (ExampleGrammar.FirstSys) in
  let sol = Solver.solve () in
  let module D = ExampleGrammar.FirstSys.D in
  let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol T);
  assert_equal (D.singleton 'b') (Solver.VH.find sol R)


let test_exercise_nullable _ =
  let module Solver = Constraint.Solver.Kleene (ExerciseGrammar.NullableSys) in
  let sol = Solver.solve () in
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal true (Solver.VH.find sol A);
  assert_equal true (Solver.VH.find sol B)

let test_exercise_first _ =
  let module Solver = Constraint.Solver.Kleene (ExerciseGrammar.FirstSys) in
  let sol = Solver.solve () in
  let module D = ExampleGrammar.FirstSys.D in
  let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol A);
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol B)


let test_example2_nullable _ =
  let module Solver = Constraint.Solver.Kleene (Example2Grammar.NullableSys) in
  let sol = Solver.solve () in
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal true (Solver.VH.find sol T);
  assert_equal true (Solver.VH.find sol R)

let test_example2_first _ =
  let module Solver = Constraint.Solver.Kleene (Example2Grammar.FirstSys) in
  let sol = Solver.solve () in
  let module D = ExampleGrammar.FirstSys.D in
  let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol T);
  assert_equal (D.singleton 'b') (Solver.VH.find sol R)


let test_large_nullable _ =
  let module Solver = Constraint.Solver.Kleene (LargeGrammar.NullableSys) in
  let sol = Solver.solve () in
  let assert_equal = assert_equal ~printer:string_of_bool in
  assert_equal false (Solver.VH.find sol N);
  assert_equal false (Solver.VH.find sol A);
  assert_equal false (Solver.VH.find sol B);
  assert_equal false (Solver.VH.find sol C)

let test_large_first _ =
  let module Solver = Constraint.Solver.Kleene (LargeGrammar.FirstSys) in
  let sol = Solver.solve () in
  let module D = ExampleGrammar.FirstSys.D in
  let assert_equal = assert_equal ~cmp:D.equal ~printer:D.show in
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol N);
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol A);
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol B);
  assert_equal (D.add 'a' (D.singleton 'b')) (Solver.VH.find sol C)




let tests =
  "constraint" >::: [
    "grammar" >::: [
      "example" >::: [
        "nullable" >:: test_example_nullable;
        "first" >:: test_example_first;
      ];
      "exercise" >::: [
        "nullable" >:: test_exercise_nullable;
        "first" >:: test_exercise_first;
      ];
      "example2" >::: [
        "nullable" >:: test_example2_nullable;
        "first" >:: test_example2_first;
      ];
      "large" >::: [
        "nullable" >:: test_large_nullable;
        "first" >:: test_large_first;
      ];

    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
