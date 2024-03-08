open OUnit2
open Modelcheck_program

module Env =
struct
  include Env

  (** Abifunktsioon keskkondade loomiseks. *)
  let of_list bindings = of_seq (List.to_seq bindings)
end

(** Abifunktsioon keskkondade kuvamiseks. *)
let pp_env ppf env =
  let pp_pair ppf (x, i) = Format.fprintf ppf "%s→%d" x i in
  let pp_sep ppf () = Format.fprintf ppf ", " in
  Format.fprintf ppf "{%a}" (Format.pp_print_list ~pp_sep pp_pair) (Env.bindings env)
[@@warning "-unused-value-declaration"]


module ExampleProgramChecker = Modelcheck.Checker.MakeNaive (ExampleProgram)

let test_example_correct _ =
  assert_equal ~printer:string_of_bool false (ExampleProgramChecker.is_correct ())

let test_example_all_states _ =
  let all_states = ExampleProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: ExampleProgram.point * env] state) expected (ExampleProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P5, Env.of_list [("x", 1); ("y", 4); ("z", 6)]);
  assert_mem false (P5, Env.of_list [("x", 1); ("y", 4); ("z", 5)]);
  assert_mem true (P7, Env.of_list [("x", 1); ("y", 4); ("z", 5)]);
  assert_mem true (P8, Env.of_list [("x", 2); ("y", 4); ("z", 5)]);
  assert_equal ~printer:string_of_int 1345 (ExampleProgramChecker.StateSet.cardinal all_states)

let test_example_error_states _ =
  let error_states = ExampleProgramChecker.error_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: ExampleProgram.point * env] state) expected (ExampleProgramChecker.StateSet.mem state error_states)
  in
  (* Kõik veaolekud. *)
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 0); ("z", 6)]);
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 0); ("z", 7)]);
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 0); ("z", 8)]);
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 0); ("z", 9)]);
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 0); ("z", 10)]);
  assert_equal ~printer:string_of_int 5 (ExampleProgramChecker.StateSet.cardinal error_states)


module CountUpProgramChecker = Modelcheck.Checker.MakeNaive (CountUpProgram)

let test_count_up_correct _ =
  assert_equal ~printer:string_of_bool true (CountUpProgramChecker.is_correct ())

let test_count_up_all_states _ =
  let all_states = CountUpProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: CountUpProgram.point * env] state) expected (CountUpProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P3, Env.of_list [("x", 1337); ("y", 1337)]);
  assert_mem false (P3, Env.of_list [("x", 42); ("y", 43)]);
  assert_mem true (P5, Env.of_list [("x", 43); ("y", 42)]);
  assert_mem true (P7, Env.of_list [("x", 1024); ("y", 1024)]);
  assert_mem false (P7, Env.of_list [("x", 42); ("y", 42)]);
  assert_equal ~printer:string_of_int 5528 (CountUpProgramChecker.StateSet.cardinal all_states)

let test_count_up_error_states _ =
  let error_states = CountUpProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (CountUpProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (CountUpProgram.is_error (P7, Env.of_list [("x", 1024); ("y", 1025)]))


module PowProgramChecker = Modelcheck.Checker.MakeNaive (PowProgram)

let test_pow_correct _ =
  assert_equal ~printer:string_of_bool false (PowProgramChecker.is_correct ())

let test_pow_all_states _ =
  let all_states = PowProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: PowProgram.point * env] state) expected (PowProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P8, Env.of_list [("x0", 3); ("n0", 4); ("x", 9); ("n", 2); ("y", 1)]);
  assert_mem false (P8, Env.of_list [("x0", 3); ("n0", 4); ("x", 9); ("n", 1); ("y", 1)]);
  assert_mem true (P11, Env.of_list [("x0", 3); ("n0", 3); ("x", 3); ("n", 3); ("y", 3)]);
  assert_mem true (P16, Env.of_list [("x0", 3); ("n0", 4); ("x", 81); ("n", 1); ("y", 81)]);
  assert_equal ~printer:string_of_int 367 (PowProgramChecker.StateSet.cardinal all_states)

let test_pow_error_states _ =
  let error_states = PowProgramChecker.error_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: PowProgram.point * env] state) expected (PowProgramChecker.StateSet.mem state error_states)
  in
  (* Kõik veaolekud. *)
  assert_mem true (P16, Env.of_list [("x0", 2); ("n0", 3); ("x", 16); ("n", 1); ("y", 32)]);
  assert_mem true (P16, Env.of_list [("x0", 3); ("n0", 3); ("x", 81); ("n", 1); ("y", 243)]);
  assert_mem true (P16, Env.of_list [("x0", 4); ("n0", 3); ("x", 256); ("n", 1); ("y", 1024)]);
  assert_mem true (P16, Env.of_list [("x0", 5); ("n0", 3); ("x", 625); ("n", 1); ("y", 3125)]);
  assert_mem true (P16, Env.of_list [("x0", 2); ("n0", 5); ("x", 256); ("n", 1); ("y", 2048)]);
  assert_mem true (P16, Env.of_list [("x0", 3); ("n0", 5); ("x", 6561); ("n", 1); ("y", 177147)]);
  assert_mem true (P16, Env.of_list [("x0", 4); ("n0", 5); ("x", 65536); ("n", 1); ("y", 4194304)]);
  assert_mem true (P16, Env.of_list [("x0", 5); ("n0", 5); ("x", 390625); ("n", 1); ("y", 48828125)]);
  assert_equal ~printer:string_of_int 8 (PowProgramChecker.StateSet.cardinal error_states)


module FixedPowProgramChecker = Modelcheck.Checker.MakeNaive (FixedPowProgram)

let test_fixed_pow_correct _ =
  assert_equal ~printer:string_of_bool true (FixedPowProgramChecker.is_correct ())

let test_fixed_pow_all_states _ =
  let all_states = FixedPowProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: FixedPowProgram.point * env] state) expected (FixedPowProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P8, Env.of_list [("x0", 3); ("n0", 4); ("x", 9); ("n", 2); ("y", 1)]);
  assert_mem false (P8, Env.of_list [("x0", 3); ("n0", 4); ("x", 9); ("n", 1); ("y", 1)]);
  assert_mem true (P11, Env.of_list [("x0", 3); ("n0", 3); ("x", 3); ("n", 3); ("y", 3)]);
  assert_mem true (P16, Env.of_list [("x0", 3); ("n0", 4); ("x", 81); ("n", 1); ("y", 81)]);
  assert_equal ~printer:string_of_int 313 (FixedPowProgramChecker.StateSet.cardinal all_states)

let test_fixed_pow_error_states _ =
  let error_states = FixedPowProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (FixedPowProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (FixedPowProgram.is_error (P16, Env.of_list [("x0", 3); ("n0", 3); ("y", 9)]))


module CountUpDownProgramChecker = Modelcheck.Checker.MakeNaive (CountUpDownProgram)

let test_count_up_down_correct _ =
  assert_equal ~printer:string_of_bool true (CountUpDownProgramChecker.is_correct ())

let test_count_up_down_all_states _ =
  let all_states = CountUpDownProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: CountUpDownProgram.point * env] state) expected (CountUpDownProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P4, Env.of_list [("x", 10); ("y", 5); ("n", 15)]);
  assert_mem false (P4, Env.of_list [("x", 10); ("y", 4); ("n", 15)]);
  assert_mem true (P6, Env.of_list [("x", 10); ("y", 4); ("n", 15)]);
  assert_mem true (P8, Env.of_list [("x", 0); ("y", 15); ("n", 15)]);
  assert_mem false (P8, Env.of_list [("x", -1); ("y", 15); ("n", 15)]);
  assert_equal ~printer:string_of_int 409 (CountUpDownProgramChecker.StateSet.cardinal all_states)

let test_count_up_down_error_states _ =
  let error_states = CountUpDownProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (CountUpDownProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (CountUpDownProgram.is_error (P8, Env.of_list [("x", 0); ("y", 14); ("n", 15)]))


module SumProgramChecker = Modelcheck.Checker.MakeNaive (SumProgram)

let test_sum_correct _ =
  assert_equal ~printer:string_of_bool false (SumProgramChecker.is_correct ())

let test_sum_all_states _ =
  let all_states = SumProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: SumProgram.point * env] state) expected (SumProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P4, Env.of_list [("i", 4); ("n", 5); ("sum", 6)]);
  assert_mem false (P4, Env.of_list [("i", 4); ("n", 2); ("sum", 6)]);
  assert_mem false (P4, Env.of_list [("i", 4); ("n", 5); ("sum", 10)]);
  assert_mem true (P6, Env.of_list [("i", 4); ("n", 5); ("sum", 10)]);
  assert_equal ~printer:string_of_int 348 (SumProgramChecker.StateSet.cardinal all_states)

let test_sum_error_states _ =
  let error_states = SumProgramChecker.error_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: SumProgram.point * env] state) expected (SumProgramChecker.StateSet.mem state error_states)
  in
  (* Kõik veaolekud. *)
  assert_mem true (P8, Env.of_list [("i", 1); ("n", 1); ("sum", 0)]);
  assert_mem true (P8, Env.of_list [("i", 2); ("n", 2); ("sum", 1)]);
  assert_mem true (P8, Env.of_list [("i", 3); ("n", 3); ("sum", 3)]);
  assert_mem true (P8, Env.of_list [("i", 4); ("n", 4); ("sum", 6)]);
  assert_mem true (P8, Env.of_list [("i", 5); ("n", 5); ("sum", 10)]);
  assert_mem true (P8, Env.of_list [("i", 6); ("n", 6); ("sum", 15)]);
  assert_mem true (P8, Env.of_list [("i", 7); ("n", 7); ("sum", 21)]);
  assert_mem true (P8, Env.of_list [("i", 8); ("n", 8); ("sum", 28)]);
  assert_mem true (P8, Env.of_list [("i", 9); ("n", 9); ("sum", 36)]);
  assert_mem true (P8, Env.of_list [("i", 10); ("n", 10); ("sum", 45)]);
  assert_mem true (P8, Env.of_list [("i", 11); ("n", 11); ("sum", 55)]);
  assert_mem true (P8, Env.of_list [("i", 12); ("n", 12); ("sum", 66)]);
  assert_mem true (P8, Env.of_list [("i", 13); ("n", 13); ("sum", 78)]);
  assert_mem true (P8, Env.of_list [("i", 14); ("n", 14); ("sum", 91)]);
  assert_mem true (P8, Env.of_list [("i", 15); ("n", 15); ("sum", 105)]);
  assert_equal ~printer:string_of_int 15 (SumProgramChecker.StateSet.cardinal error_states)


module FixedSumProgramChecker = Modelcheck.Checker.MakeNaive (FixedSumProgram)

let test_fixed_sum_correct _ =
  assert_equal ~printer:string_of_bool true (FixedSumProgramChecker.is_correct ())

let test_fixed_sum_all_states _ =
  let all_states = FixedSumProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: FixedSumProgram.point * env] state) expected (FixedSumProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P4, Env.of_list [("i", 4); ("n", 5); ("sum", 6)]);
  assert_mem false (P4, Env.of_list [("i", 4); ("n", 2); ("sum", 6)]);
  assert_mem false (P4, Env.of_list [("i", 4); ("n", 5); ("sum", 10)]);
  assert_mem true (P6, Env.of_list [("i", 4); ("n", 5); ("sum", 10)]);
  assert_equal ~printer:string_of_int 393 (FixedSumProgramChecker.StateSet.cardinal all_states)

let test_fixed_sum_error_states _ =
  let error_states = FixedSumProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (FixedSumProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (FixedSumProgram.is_error (P8, Env.of_list [("i", 15); ("n", 15); ("sum", 105)]))


module LinearSqrtProgramChecker = Modelcheck.Checker.MakeNaive (LinearSqrtProgram)

let test_linear_sqrt_correct _ =
  assert_equal ~printer:string_of_bool true (LinearSqrtProgramChecker.is_correct ())

let test_linear_sqrt_all_states _ =
  let all_states = LinearSqrtProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: LinearSqrtProgram.point * env] state) expected (LinearSqrtProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P5, Env.of_list [("y", 0); ("l", 0)]);
  assert_mem true (P5, Env.of_list [("y", 1); ("l", 1)]);
  assert_mem true (P5, Env.of_list [("y", 4); ("l", 2)]);
  assert_mem true (P5, Env.of_list [("y", 20); ("l", 4)]);
  assert_mem false (P5, Env.of_list [("y", 100); ("l", 10)]);
  assert_equal ~printer:string_of_int 537 (LinearSqrtProgramChecker.StateSet.cardinal all_states)

let test_linear_sqrt_error_states _ =
  let error_states = LinearSqrtProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (LinearSqrtProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (LinearSqrtProgram.is_error (P5, Env.of_list [("y", 10); ("l", 4)]))


module BinarySqrtProgramChecker = Modelcheck.Checker.MakeNaive (BinarySqrtProgram)

let test_binary_sqrt_correct _ =
  assert_equal ~printer:string_of_bool true (BinarySqrtProgramChecker.is_correct ())

let test_binary_sqrt_all_states _ =
  let all_states = BinarySqrtProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: BinarySqrtProgram.point * env] state) expected (BinarySqrtProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P12, Env.of_list [("y", 0); ("l", 0); ("r", 1)]);
  assert_mem true (P12, Env.of_list [("y", 1); ("l", 1); ("m", 1); ("r", 2)]);
  assert_mem true (P12, Env.of_list [("y", 4); ("l", 2); ("m", 3); ("r", 3)]);
  assert_mem true (P12, Env.of_list [("y", 20); ("l", 4); ("m", 4); ("r", 5)]);
  assert_mem false (P12, Env.of_list [("y", 100); ("l", 10); ("m", 11); ("r", 11)]);
  assert_equal ~printer:string_of_int 1046 (BinarySqrtProgramChecker.StateSet.cardinal all_states)

let test_binary_sqrt_error_states _ =
  let error_states = BinarySqrtProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (BinarySqrtProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (BinarySqrtProgram.is_error (P12, Env.of_list [("y", 10); ("l", 4)]))


module NewtonSqrtProgramChecker = Modelcheck.Checker.MakeNaive (NewtonSqrtProgram)

let test_newton_sqrt_correct _ =
  assert_equal ~printer:string_of_bool true (NewtonSqrtProgramChecker.is_correct ())

let test_newton_sqrt_all_states _ =
  let all_states = NewtonSqrtProgramChecker.all_states () in
  let assert_mem expected state =
    assert_equal ~printer:string_of_bool ~msg:([%show: NewtonSqrtProgram.point * env] state) expected (NewtonSqrtProgramChecker.StateSet.mem state all_states)
  in
  assert_mem true (P13, Env.of_list [("s", 0); ("x0", 0)]);
  assert_mem true (P13, Env.of_list [("s", 1); ("x0", 1)]);
  assert_mem true (P13, Env.of_list [("s", 4); ("x0", 2); ("x1", 2)]);
  assert_mem true (P13, Env.of_list [("s", 20); ("x0", 4); ("x1", 4)]);
  assert_mem false (P13, Env.of_list [("s", 100); ("x0", 10); ("x1", 10)]);
  assert_equal ~printer:string_of_int 558 (NewtonSqrtProgramChecker.StateSet.cardinal all_states)

let test_newton_sqrt_error_states _ =
  let error_states = NewtonSqrtProgramChecker.error_states () in
  assert_equal ~printer:string_of_int 0 (NewtonSqrtProgramChecker.StateSet.cardinal error_states);
  (* Kontrollime veaoleku funktsiooni hüpoteetilisel olekul. *)
  assert_equal ~printer:string_of_bool true (NewtonSqrtProgram.is_error (P13, Env.of_list [("s", 10); ("x0", 4)]))


let tests =
  "modelcheck" >::: [
    "program" >::: [
      "example" >::: [
        "correct" >:: test_example_correct;
        "all_states" >:: test_example_all_states;
        "error_states" >:: test_example_error_states;
      ];
      "count_up" >::: [
        "correct" >:: test_count_up_correct;
        "all_states" >:: test_count_up_all_states;
        "error_states" >:: test_count_up_error_states;
      ];
      "pow" >::: [
        "correct" >:: test_pow_correct;
        "all_states" >:: test_pow_all_states;
        "error_states" >:: test_pow_error_states;
        "fixed" >::: [
          "correct" >:: test_fixed_pow_correct;
          "all_states" >:: test_fixed_pow_all_states;
          "error_states" >:: test_fixed_pow_error_states;
        ];
      ];
      "count_up_down" >::: [
        "correct" >:: test_count_up_down_correct;
        "all_states" >:: test_count_up_down_all_states;
        "error_states" >:: test_count_up_down_error_states;
      ];
      "sum" >::: [
        "correct" >:: test_sum_correct;
        "all_states" >:: test_sum_all_states;
        "error_states" >:: test_sum_error_states;
        "fixed" >::: [
          "correct" >:: test_fixed_sum_correct;
          "all_states" >:: test_fixed_sum_all_states;
          "error_states" >:: test_fixed_sum_error_states;
        ];
      ];
      "sqrt" >::: [
        "linear" >::: [
          "correct" >:: test_linear_sqrt_correct;
          "all_states" >:: test_linear_sqrt_all_states;
          "error_states" >:: test_linear_sqrt_error_states;
        ];
        "binary" >::: [
          "correct" >:: test_binary_sqrt_correct;
          "all_states" >:: test_binary_sqrt_all_states;
          "error_states" >:: test_binary_sqrt_error_states;
        ];
        "newton" >::: [
          "correct" >:: test_newton_sqrt_correct;
          "all_states" >:: test_newton_sqrt_all_states;
          "error_states" >:: test_newton_sqrt_error_states;
        ];
      ];
    ];
  ]

let () = run_test_tt_main (OUnitTodo.wrap tests)
