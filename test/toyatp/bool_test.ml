open OUnit2
open Toylangs.Bool
open Toyatp.Bool

let show_verdict = function
  | Satisfiable env -> "Satisfiable " ^ ([%show: char list] (CharSet.elements env))
  | Unsatisfiable -> "Unsatisfiable"
  | Unknown -> "Unknown"

(** Kontrollib, et avaldis oleks mitte-kehtestatav. *)
let assert_unsatisfiable e =
  assert_equal ~printer:show_verdict Unsatisfiable (check e)

(** Kontrollib. et avaldis oleks kehtestatav.
    Lisaks kontrollib, et tulemusena antud keskkonnas avaldis päriselt väärtustuks ka tõeseks.
    Vt. Toylangs.Bool.eval *)
let assert_satisfiable e =
  match check e with
  | Satisfiable env ->
    assert_equal ~printer:string_of_bool true (eval env e)
  | _ ->
    assert_failure "expected Satisfiable"


let test_const _ =
  assert_satisfiable (Var 'A')

let test_not _ =
  assert_satisfiable (Not (Var 'A'));
  assert_satisfiable (Not (Not (Var 'A')))

let test_or _ =
  assert_satisfiable (Or (Var 'A', Var 'B'));
  assert_satisfiable (Or (Var 'A', Var 'A'));
  assert_satisfiable (Or (Var 'A', Not (Var 'A')));
  assert_satisfiable (Or (Not (Var 'A'), Not (Var 'A')))

let test_and _ =
  (* Simuleerime konjunktsiooni De Morgani kaudu. *)
  assert_satisfiable (Not (Or (Var 'A', Var 'B')));
  assert_unsatisfiable (Not (Or (Var 'A', Not (Var 'A'))));
  assert_satisfiable (Not (Or (Not (Var 'A'), Not (Var 'B'))))

let test_imp _ =
  assert_satisfiable (Imp (Var 'A', Var 'B'));
  assert_satisfiable (Imp (Var 'A', Var 'A'));
  assert_satisfiable (Imp (Var 'A', Not (Var 'A')));
  assert_satisfiable (Imp (Not (Var 'A'), Var 'A'));
  assert_satisfiable (Imp (Not (Var 'A'), Not (Var 'A')))

let test_and' _ =
  (* Simuleerime konjunktsiooni implikatsiooni eituse kaudu. *)
  assert_satisfiable (Not (Imp (Var 'A', Var 'B')));
  assert_satisfiable (Not (Imp (Var 'A', Not (Var 'B'))));
  assert_satisfiable (Not (Imp (Var 'A', Not (Var 'A'))));
  assert_unsatisfiable (Not (Imp (Var 'A', Var 'A')))

let tests =
  "bool" >::: [
    "const" >:: test_const;
    "not" >:: test_not;
    "or" >:: test_or;
    "and" >:: test_and;
    "imp" >:: test_imp;
    "and'" >:: test_and';
  ]
