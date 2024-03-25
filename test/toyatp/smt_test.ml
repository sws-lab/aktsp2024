open OUnit2
open Z3
open Toyatp.Smt

(** SatExample test. *)
let test_sat_example _ =
  let open SatExample in
  (* Kontrollime OCaml-i arvutustega, kas leitud väärtustus tõesti muutab need kolm võrrandit tõesteks. *)
  (* Kuna sobivaid väärtustusi võib üldiselt olla mitu,
     siis me ei kontrolli muutujate väärtusi (ehk väärtustust) otse,
     vaid lihtsalt kontrollime nõutud võrrandeid. *)
  assert_bool "🟢 + 🟢 = 10" (circle + circle = 10);
  assert_bool "🟢*🟨 + 🟨 = 12" (circle * square + square = 12);
  assert_bool "🟢*🟨 - 🔺*🟢 = 🟢" (circle * square - triangle * circle = circle)

(** ProofExample test. *)
let test_proof_example _ =
  (* Tõestuse korral mingit väärtustust pole, seega saame kontrollida ainult Z3 tulemust.
     Seega peame ise veenduma, et tõestasime õiget asja,
     st teisendasime implikatsiooni korrektselt Z3 avaldiseks. *)
  assert_equal ~printer:Solver.string_of_status UNSATISFIABLE ProofExample.status

let tests =
  "smt" >::: [
    "sat_example" >:: test_sat_example;
    "proof_example" >:: test_proof_example;
  ]
