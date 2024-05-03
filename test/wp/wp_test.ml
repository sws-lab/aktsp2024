open OUnit2
open Z3
open Ast
open Symbolic
open Wp

(** Kontrollib, et Z3 avaldised oleksid ekvivalentsed. *)
let assert_equivalent (expected: Expr.expr) (actual: Expr.expr): unit =
  let formula = Syntax.(not (iff expected actual)) in
  match Solver.check solver [formula] with
  | SATISFIABLE ->
    let model = Option.get (Solver.get_model solver) in
    let actual = Expr.simplify actual None in (* Lihtsustame loetavuse mõttes *)
    assert_failure (Format.sprintf "%s NOT EQUIVALENT %s: %s" (Expr.to_string expected) (Expr.to_string actual) (Model.to_string model))
  | UNSATISFIABLE ->
    ()
  | UNKNOWN ->
    assert_failure "unknown"

let test_wp_nop _ =
  assert_equivalent Syntax.true_ (wp Nop Syntax.true_);
  assert_equivalent Syntax.false_ (wp Nop Syntax.false_);
  assert_equivalent Syntax.(!"X" = ~$5) (wp Nop Syntax.(!"X" = ~$5));
  assert_equivalent Syntax.(!"x" >= ~$0) (wp Nop Syntax.(!"x" >= ~$0))

let test_wp_error _ =
  assert_equivalent Syntax.false_ (wp Error Syntax.true_);
  assert_equivalent Syntax.false_ (wp Error Syntax.false_);
  assert_equivalent Syntax.false_ (wp Error Syntax.(!"x" >= ~$0))

let test_wp_assign_simple _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      "X" := ~$5
    ) Syntax.(!"X" = ~$5));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      "X" := !"Y"
    ) Syntax.(!"X" = !"Y"));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      "X" := ~$3
    ) Syntax.(~$0 <= !"X" && !"X" <= ~$5));

  assert_equivalent Syntax.false_ (wp Ast.Syntax.(
      "X" := ~$5
    ) Syntax.(!"X" = ~$0))

let test_wp_assign_subst _ =
  assert_equivalent Syntax.(!"Y" = ~$1) (wp Ast.Syntax.(
      "X" := !"Y"
    ) Syntax.(!"X" = ~$1));
  assert_equivalent Syntax.(!"Y" <= ~$4) (wp Ast.Syntax.(
      "X" := !"Y" + ~$1
    ) Syntax.(!"X" <= ~$5));
  assert_equivalent Syntax.(!"a" + ~$3 > ~$0) (wp Ast.Syntax.(
      "b" := !"a" + ~$3
    ) Syntax.(!"b" > ~$0));
  assert_equivalent Syntax.(!"Y" + !"Z" = ~$5) (wp Ast.Syntax.(
      "X" := !"Y" + !"Z"
    ) Syntax.(!"X" = ~$5))

let test_wp_assign_self _ =
  assert_equivalent Syntax.(!"X" = ~$2) (wp Ast.Syntax.(
      "X" := !"X" + ~$1
    ) Syntax.(!"X" = ~$3));
  assert_equivalent Syntax.(!"X" + ~$1 <= ~$5) (wp Ast.Syntax.(
      "X" := !"X" + ~$1
    ) Syntax.(!"X" <= ~$5));
  assert_equivalent Syntax.(!"X" < ~$4) (wp Ast.Syntax.(
      "X" := !"X" + ~$1
    ) Syntax.(!"X" < ~$5));
  assert_equivalent Syntax.(!"x" > ~$15) (wp Ast.Syntax.(
      "x" := !"x" - ~$5
    ) Syntax.(!"x" > ~$10));
  assert_equivalent Syntax.(~$2 * !"X" <= ~$10) (wp Ast.Syntax.(
      "X" := ~$2 * !"X"
    ) Syntax.(!"X" <= ~$10));
  assert_equivalent Syntax.(!"X" + !"Y" = ~$1) (wp Ast.Syntax.(
      "X" := !"X" + !"Y"
    ) Syntax.(!"X" = ~$1));
  assert_equivalent Syntax.(!"x" * !"x" < ~$2 * !"y") (wp Ast.Syntax.(
      "x" := !"x" * !"x" - !"y"
    ) Syntax.(!"x" < !"y"))

let test_wp_assign_free _ =
  assert_equivalent Syntax.(!"X" = !"m") (wp Ast.Syntax.(
      "X" := !"X" + ~$1
    ) Syntax.(!"X" = !"m" + ~$1))

let test_wp_seq_simple _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        "X" := ~$5;
        "Y" := ~$0;
      ]
    ) Syntax.(!"X" = ~$5));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        "X" := ~$1;
        "Y" := ~$2;
      ]
    ) Syntax.(!"X" = ~$1 && !"Y" = ~$2))

let test_wp_seq_order _ =
  assert_equivalent Syntax.(!"x" + ~$1 = ~$43) (wp Ast.Syntax.(
      seq [
        "y" := !"x" + ~$1;
        "z" := !"y";
      ]
    ) Syntax.(!"z" = ~$43));
  assert_equivalent Syntax.(!"x" > ~$15) (wp Ast.Syntax.(
      seq [
        "x" := !"x" - ~$5;
        "x" := !"x" * ~$2
      ]
    ) Syntax.(!"x" > ~$20));
  assert_equivalent Syntax.(!"x" + !"y" = ~$42) (wp Ast.Syntax.(
      seq [
        "z" := !"x";
        "z" := !"z" + !"y";
        "u" := !"z";
      ]
    ) Syntax.(!"u" = ~$42));
  assert_equivalent Syntax.(!"X" <= !"Y") (wp Ast.Syntax.(
      seq [
        "X" := !"X" + !"Y";
        "Y" := !"X" - !"Y";
        "X" := !"X" - !"Y";
      ]
    ) Syntax.(!"Y" <= !"X"))

let test_wp_seq_free _ =
  assert_equivalent Syntax.(!"a" = !"n") (wp Ast.Syntax.(
      seq [
        "X" := !"a";
        Nop;
      ]
    ) Syntax.(!"X" = !"n"));
  assert_equivalent Syntax.(!"X" = !"m" && !"Y" = !"n") (wp Ast.Syntax.(
      seq [
        "X" := !"X" + !"Y";
        "Y" := !"X" - !"Y";
        "X" := !"X" - !"Y";
      ]
    ) Syntax.(!"X" = !"n" && !"Y" = !"m"))

let test_wp_if_simple _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      if_ (!"X" < !"Y")
        ("Z" := !"Y" - !"X")
        ("Y" := !"X" + !"Z")
    ) Syntax.(!"Y" = !"X" + !"Z"));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      if_ (!"X" <= !"Y")
        ("Z" := !"Y" - !"X")
        ("Z" := !"X" - !"Y")
    ) Syntax.(!"Z" + !"X" = !"Y" || !"Z" + !"Y" = !"X"))

let test_wp_if_refine _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      if_ (!"x" < !"y")
        ("x" := !"y")
        Nop
    ) Syntax.(!"x" >= !"y"));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      if_ (~$0 <= !"i")
        ("r" := !"i")
        ("r" := ~$0 - !"i")
    ) Syntax.(~$0 <= !"r"));
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      if_ (!"X" = ~$0)
        ("Y" := ~$2)
        ("Y" := !"X" + ~$1)
    ) Syntax.(!"X" <= !"Y"));
  assert_equivalent Syntax.(!"X" + !"Y" = !"Z") (wp Ast.Syntax.(
      if_ (!"Y" <> ~$0)
        ("X" := !"X" + !"Y")
        Nop
    ) Syntax.(!"X" = !"Z"));
  assert_equivalent Syntax.(implies (!"X" = ~$0) (!"Z" = ~$4) && implies (!"X" <> ~$0) (!"W" = ~$3)) (wp Ast.Syntax.(
      if_ (!"X" = ~$0)
        ("Y" := !"Z" + ~$1)
        ("Y" := !"W" + ~$2)
    ) Syntax.(!"Y" = ~$5));
  assert_equivalent Syntax.(!"x" >= ~$(-1)) (wp Ast.Syntax.(
      if_ (!"x" < ~$15)
        ("x" := !"x" + ~$1)
        ("x" := ~$0)
    ) Syntax.(~$0 <= !"x" && !"x" <= ~$15))

let test_wp_if_free _ =
  assert_equivalent Syntax.(!"x" + !"y" = !"m") (wp Ast.Syntax.(
      if_ (!"x" > !"y")
        (seq [
          "a" := !"x";
          "x" := !"y";
          "y" := !"a";
        ])
        Nop
    ) Syntax.(!"x" <= !"y" && !"x" + !"y" = !"m"))

let test_wp_assert _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        "x" := ~$1;
        "y" := !"x" + ~$2;
        assert_ (!"y" = ~$3);
      ]
    ) Syntax.true_);
  assert_equivalent Syntax.(!"x" > ~$1) (wp Ast.Syntax.(
      seq [
        "y" := !"x" + ~$2;
        assert_ (!"y" > ~$3);
      ]
    ) Syntax.true_);
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        if_ (~$0 <= !"i")
          ("r" := !"i")
          ("r" := ~$0 - !"i");
        assert_ (!"r" >= ~$0)
      ]
    ) Syntax.true_)

let test_wp_while _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(while_ ~$1 Nop) Syntax.true_);
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(while_ ~$1 Nop) Syntax.false_);
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(while_ ~$1 Nop) Syntax.(!"x" >= ~$0))
  (* Keerukamaid While lauseid pole vaja toetada. *)

let test_wp_assume _ =
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        assume (!"X" = ~$1);
        "X" := !"X" + ~$1;
        assert_ (!"X" = ~$2);
      ]
    ) Syntax.true_);
  assert_equivalent Syntax.true_ (wp Ast.Syntax.(
      seq [
        assume (!"x" > ~$2);
        "y" := !"x" * ~$2;
        "z" := !"x" + ~$2;
        assert_ (!"y" > !"z");
      ]
    ) Syntax.true_);
  assert_equivalent Syntax.(!"x" <= ~$1 || !"x" > ~$2) (wp Ast.Syntax.(
      seq [
        assume (!"x" > ~$1);
        "y" := !"x" * ~$2;
        "z" := !"x" + ~$2;
        assert_ (!"y" > !"z");
      ]
    ) Syntax.true_)


(** Kontrollib, et programm oleks korrektne. *)
let assert_correct stmt =
  assert_equal ~printer:show_verdict Correct (check stmt)

(** Kontrollib. et programm oleks vigane.
    Lisaks kontrollib, et tulemusena antud keskkonnas programmi täitmine päriselt annaks ka vea.
    Vt. Eval.Concrete.eval_stmt. *)
let assert_incorrect stmt =
  let failure_oracle (_, _) = failwith "failure_oracle" in
  match check stmt with
  | Incorrect env ->
    OUnitTodo.assert_raises (Failure "eval_stmt: Error") (fun () -> Eval.Concrete.eval_stmt env failure_oracle stmt)
  | _ ->
    assert_failure "expected Incorrect"

let test_check_correct _ =
  let stmt = Ast.Syntax.(
    seq [
      assume (!"x" > ~$2);
      "y" := !"x" * ~$2;
      "z" := !"x" + ~$2;
      assert_ (!"y" > !"z");
    ]
  )
  in
  assert_correct stmt

let test_check_incorrect _ =
  let stmt = Ast.Syntax.(
    seq [
      assume (!"x" > ~$1);
      "y" := !"x" * ~$2;
      "z" := !"x" + ~$2;
      assert_ (!"y" > !"z");
    ]
  )
  in
  assert_incorrect stmt

let test_check_abs _ =
  let stmt = Ast.Syntax.(
    seq [
      if_ (~$0 <= !"i")
        ("r" := !"i")
        ("r" := ~$0 - !"i");
      assert_ (!"r" >= ~$0)
    ]
  )
  in
  assert_correct stmt

let test_check_swap _ =
  let stmt = Ast.Syntax.(
    seq [
      assume (!"X" = !"m");
      assume (!"Y" = !"n");
      "X" := !"X" + !"Y";
      "Y" := !"X" - !"Y";
      "X" := !"X" - !"Y";
      assert_ (!"X" = !"n");
      assert_ (!"Y" = !"m");
    ]
  )
  in
  assert_correct stmt

let test_check_example_modelcheck _ =
  let stmt = Ast.Syntax.(
    seq [
      "x" := ~$1;
      if_ (!"z" > ~$5)
        ("x" := !"y")
        ("x" := !"x" + ~$1);
      assert_ (!"x" <> ~$0);
    ]
  )
  in
  assert_incorrect stmt

let test_check_example_abseval1 _ =
  let stmt = Ast.Syntax.(
    seq [
      if_ (!"x" > ~$5)
        ("x" := ~$2 * !"x")
        ("x" := !"x" + ~$5);
      assert_ (!"x" <> ~$0);
    ]
  )
  in
  assert_incorrect stmt

let test_check_example_abseval2 _ =
  let stmt = Ast.Syntax.(
    seq [
      if_ (!"x" > ~$5)
        ("x" := ~$2 * !"x")
        ("x" := ~$9 - !"x");
      assert_ (!"x" <> ~$0);
    ]
  )
  in
  assert_correct stmt

let test_check_example_abseval3 _ =
  let stmt = Ast.Syntax.(
    seq [
      if_ (!"x" > ~$5)
        ("x" := ~$2 * !"x")
        ("x" := !"x" - ~$9);
      assert_ (!"x" <> ~$0);
    ]
  )
  in
  assert_correct stmt


let tests =
  "wp" >::: [
    "wp" >::: [
      "nop" >:: test_wp_nop;
      "error" >:: test_wp_error;
      "assign" >::: [
        "simple" >:: test_wp_assign_simple;
        "subst" >:: test_wp_assign_subst;
        "self" >:: test_wp_assign_self;
        "free" >:: test_wp_assign_free;
      ];
      "seq" >::: [
        "simple" >:: test_wp_seq_simple;
        "order" >:: test_wp_seq_order;
        "free" >:: test_wp_seq_free;
      ];
      "if" >::: [
        "simple" >:: test_wp_if_simple;
        "refine" >:: test_wp_if_refine;
        "free" >:: test_wp_if_free;
      ];
      "assert" >:: test_wp_assert;
      "while" >:: test_wp_while;
      "assume" >:: test_wp_assume;
    ];
    "check" >::: [
      "correct" >:: test_check_correct;
      "incorrect" >:: test_check_incorrect;
      "abs" >:: test_check_abs;
      "swap" >:: test_check_swap;
      "example_modelcheck" >:: test_check_example_modelcheck;
      "example_abseval1" >:: test_check_example_abseval1;
      "example_abseval2" >:: test_check_example_abseval2;
      "example_abseval3" >:: test_check_example_abseval3;
    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
