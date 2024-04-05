open OUnit2
open Z3
open Toyatp.Sat

(** SatExample test. *)
let test_sat_example _ =
  let open SatExample in
  (* Kontrollime OCaml-i arvutustega, kas leitud väärtustus tõesti muutab need kolm valemit tõesteks. *)
  (* Kuna sobivaid väärtustusi võib üldiselt olla mitu,
     siis me ei kontrolli muutujate väärtusi (ehk väärtustust) otse,
     vaid lihtsalt kontrollime nõutud valemeid. *)
  assert_bool "P → Q" (Crashcourse.Basics.implies p q);
  assert_bool "R ↔ ¬Q" (r = not q);
  assert_bool "¬P ∨ R" (not p || r)

(** ProofExample test. *)
let test_proof_example _ =
  (* Tõestuse korral mingit väärtustust pole, seega saame kontrollida ainult Z3 tulemust.
     Seega peame ise veenduma, et tõestasime õiget asja,
     st teisendasime lausearvutuse samaväärsuse korrektselt Z3 avaldiseks. *)
  assert_equal ~printer:Solver.string_of_status UNSATISFIABLE ProofExample.status

let tests =
  "sat" >::: [
    "sat_example" >:: test_sat_example;
    "proof_example" >:: test_proof_example;
  ]
