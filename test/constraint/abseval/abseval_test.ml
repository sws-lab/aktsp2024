open OUnit2
open Ast
open Constraint_abseval

module MakeEvalStmt (ID: IntDomain.S) =
struct
  include Make (ID)

  (** Väärtustab lause keskkonnas.
      Kasutab võrrandisüsteemi lahendamist. *)
  let eval_stmt (env: ED.t) (stmt: stmt): ED.t =
    let cfg = Cfg.of_stmt stmt in (* Loome juhtvoograafi. *)
    (* Loome võrrandisüsteemi. *)
    let module Sys = MakeSys (struct
        let cfg = cfg
        let entry_env = env (* Keskkond juhtvoograafi entry tipus. *)
      end)
    in
    let module Solver = Constraint.Solver.Kleene (Sys) in (* Loome solveri. *)
    let sol = Solver.solve () in (* Lahendame võrrandisüsteemi. *)
    (* Tagastame lahendist juhtvoograafi exit tipu keskkonna.
       See vastab kogu lause väärtustamise tulemusele. *)
    Solver.VH.find sol cfg.exit
end

(** Kopeeritud Abseval.eval_stmt testid. *)

module Common_test (ID: IntDomain.S) =
struct
  open MakeEvalStmt (ID)

  let test_eval_stmt_nop _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 4)) (eval_stmt (ED.singleton "x" (ID.of_int 4))
        Nop
      );
    assert_equal ED.bot (eval_stmt ED.bot
        Nop
      )

  let test_eval_stmt_assign _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 5)) (eval_stmt (ED.singleton "x" (ID.of_int 4)) (
        Assign ("x", Num 5)
      ));
    assert_equal (ED.add "y" (ID.of_int 42) (ED.singleton "x" (ID.of_int 5))) (eval_stmt (ED.add "y" (ID.of_int 42) (ED.singleton "x" (ID.of_int 4))) (
        Assign ("x", Num 5)
      ));
    assert_equal (ED.singleton "x" (ID.of_int 5)) (eval_stmt ED.empty (
        Assign ("x", Num 5)
      ));
    assert_equal ED.bot (eval_stmt ED.bot (
        Assign ("x", Num 5)
      ))

  let test_eval_stmt_seq _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.add "y" (ID.of_int 10) (ED.singleton "x" (ID.of_int 5))) (eval_stmt ED.empty (
        Seq (
          Assign ("x", Num 5),
          Assign ("y", Binary (Var "x", Add, Num 5))
        )
      ));
    assert_equal ED.bot (eval_stmt ED.bot (
        Seq (
          Assign ("x", Num 5),
          Assign ("y", Binary (Var "x", Add, Num 5))
        )
      ))

  let test_eval_stmt_if_join _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.join (ID.of_int 5) (ID.of_int 10))) (eval_stmt ED.empty (
        If (Rand (0, 1),
          Assign ("x", Num 5),
          Assign ("x", Num 10)
        )
      ));
    assert_equal ED.bot (eval_stmt ED.bot (
        If (Rand (0, 1),
          Assign ("x", Num 5),
          Assign ("x", Num 10)
        )
      ))

  let test_eval_stmt_if_dead _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 5)) (eval_stmt ED.empty (
        If (Num 1,
          Assign ("x", Num 5),
          Assign ("x", Num 10)
        )
      ));
    assert_equal (ED.singleton "x" (ID.of_int 10)) (eval_stmt ED.empty (
        If (Num 0,
          Assign ("x", Num 5),
          Assign ("x", Num 10)
        )
      ))

  let test_eval_stmt_if_refine _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    (* Samad kitsendamised, nagu test_eval_stmt_while_refine. *)
    assert_equal (ED.add "y" (ID.of_int 10) (ED.singleton "x" (ID.of_interval (0, 10)))) (eval_stmt (ED.singleton "x" (ID.of_interval (0, 10))) (
        If (Binary (Var "x", Ne, Num 10),
          Nop,
          Assign ("y", Var "x")
        )
      ));
    assert_equal (ED.add "y" (ID.of_interval (0, 9)) (ED.singleton "x" (ID.of_interval (0, 10)))) (eval_stmt (ED.singleton "x" (ID.of_interval (0, 10))) (
        If (Binary (Var "x", Ne, Num 10),
          Assign ("y", Var "x"),
          Nop
        )
      ));
    (* Samad kitsendamised, nagu test_eval_stmt_while_decr. *)
    assert_equal (ED.add "y" (ID.of_int 0) (ED.singleton "x" (ID.of_interval (0, 10)))) (eval_stmt (ED.singleton "x" (ID.of_interval (0, 10))) (
        If (Binary (Var "x", Ne, Num 0),
          Nop,
          Assign ("y", Var "x")
        )
      ));
    assert_equal (ED.add "y" (ID.of_interval (1, 10)) (ED.singleton "x" (ID.of_interval (0, 10)))) (eval_stmt (ED.singleton "x" (ID.of_interval (0, 10))) (
        If (Binary (Var "x", Ne, Num 0),
          Assign ("y", Var "x"),
          Nop
        )
      ))

  let test_eval_stmt_while_simple _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 0)) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Num 0,
          Nop
        )
      ));
    assert_equal ED.bot (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Num 1,
          Nop
        )
      ));
    assert_equal (ED.singleton "x" (ID.of_int 0)) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Num 0,
          Assign ("x", Binary (Var "x", Add, Num 1))
        )
      ));
    assert_equal (ED.singleton "x" (ID.join (ID.of_int 0) (ID.of_int 5))) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Rand (0, 1),
          Assign ("x", Num 5)
        )
      ));
    assert_equal ED.bot (eval_stmt ED.bot (
        While (Rand (0, 1),
          Assign ("x", Binary (Var "x", Add, Num 1))
        )
      ))

  let test_eval_stmt_while_refine ctxt =
    OUnitTodo.skip_if_fails test_eval_stmt_if_refine ctxt "kui kitsendamine ei tööta, siis see test ei termineeruks";

    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 10)) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Binary (Var "x", Ne, Num 10),
          Assign ("x", Binary (Var "x", Add, Num 1))
        )
      ));
    assert_equal (ED.add "y" (ID.of_interval (0, 9)) (ED.singleton "x" (ID.of_int 10))) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Binary (Var "x", Ne, Num 10),
          Seq (
            Assign ("y", Var "x"),
            Assign ("x", Binary (Var "x", Add, Num 1))
          )
        )
      ))

  let test_eval_stmt_while_decr ctxt =
    OUnitTodo.skip_if_fails test_eval_stmt_if_refine ctxt "kui kitsendamine ei tööta, siis see test ei termineeruks";

    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    assert_equal (ED.singleton "x" (ID.of_int 0)) (eval_stmt (ED.singleton "x" (ID.of_int 10)) (
        While (Binary (Var "x", Ne, Num 0),
          Assign ("x", Binary (Var "x", Sub, Num 1))
        )
      ));
    assert_equal (ED.add "y" (ID.of_interval (1, 10)) (ED.singleton "x" (ID.of_int 0))) (eval_stmt (ED.singleton "x" (ID.of_int 10)) (
        While (Binary (Var "x", Ne, Num 0),
          Seq (
            Assign ("y", Var "x"),
            Assign ("x", Binary (Var "x", Sub, Num 1))
          )
        )
      ))

  let test_eval_stmt_error _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    OUnitTodo.assert_raises (Failure "eval_edge: Error") (fun () -> eval_stmt ED.empty
        Error
      );
    assert_equal ED.bot (eval_stmt ED.bot
        Error
      )

  let tests =
    "common" >::: [
      "eval_stmt" >::: [
        "nop" >:: test_eval_stmt_nop;
        "assign" >:: test_eval_stmt_assign;
        "seq" >:: test_eval_stmt_seq;
        "if" >::: [
          "join" >:: test_eval_stmt_if_join;
          "dead" >:: test_eval_stmt_if_dead;
          "refine" >:: test_eval_stmt_if_refine;
        ];
        "while" >::: [
          "simple" >:: test_eval_stmt_while_simple;
          "refine" >:: test_eval_stmt_while_refine;
          "decr" >:: test_eval_stmt_while_decr;
        ];
        "error" >:: test_eval_stmt_error;
      ];
    ]
end

module Set_test =
struct
  module ID = IntDomain.Set
  open MakeEvalStmt (ID)

  let test_eval_stmt_if_refine_precise _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    (* Täpsed ainult Set-iga (mitte Flat-i ja Interval-iga),
       sest teised abstraheerivad ega suuda välistada vahepealseid väärtusi. *)
    assert_equal (ED.singleton "x" (ID.of_int 10)) (eval_stmt (ED.singleton "x" (ID.join (ID.of_int 0) (ID.of_int 5))) (
        If (Var "x",
          Assign ("x", Binary (Var "x", Add, Num 5)),
          Assign ("x", Binary (Var "x", Add, Num 10))
        )
      ));
    assert_equal (ED.singleton "x" (ID.of_int 10)) (eval_stmt (ED.singleton "x" (ID.join (ID.of_int 0) (ID.of_int 5))) (
        If (Binary (Var "x", Ne, Num 5),
          Assign ("x", Binary (Var "x", Add, Num 10)),
          Assign ("x", Binary (Var "x", Add, Num 5))
        )
      ))

  module Common = Common_test (ID)
  let tests =
    "set" >::: [
      Common.tests;
      "eval_stmt" >::: [
        "if" >::: [
          "refine_precise" >:: test_eval_stmt_if_refine_precise;
        ];
      ];
    ]
end

module Flat_test =
struct
  module ID = IntDomain.Flat
  open MakeEvalStmt (ID)

  let test_eval_stmt_while_nonterm _ =
    let assert_equal = assert_equal ~cmp:ED.equal ~printer:ED.show in
    (* Termineeruvad ainult Flat-iga (mitte Set-i ja Interval-iga),
       sest teistega kasvab võimalike väärtuste hulk/vahemik ükshaaval tõkestamatult. *)
    assert_equal ED.bot (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Num 1,
          Assign ("x", Binary (Var "x", Add, Num 1))
        )
      ));
    assert_equal (ED.singleton "x" Top) (eval_stmt (ED.singleton "x" (ID.of_int 0)) (
        While (Rand (0, 1),
          Assign ("x", Binary (Var "x", Add, Num 1))
        )
      ))

  module Common = Common_test (ID)
  let tests =
    "flat" >::: [
      Common.tests;
      "eval_stmt" >::: [
        "while" >::: [
          "nonterm" >:: test_eval_stmt_while_nonterm;
        ];
      ];
    ]
end

module Interval_test =
struct
  module ID = IntDomain.Interval
  open MakeEvalStmt (ID)

  module Common = Common_test (ID)
  let tests =
    "interval" >::: [
      Common.tests;
    ]
end

let tests =
  "constraint" >::: [
    "abseval" >::: [
      Set_test.tests;
      Flat_test.tests;
      Interval_test.tests;
    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
