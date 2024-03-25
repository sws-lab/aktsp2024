open OUnit2
open Z3
open Toyatp.Magic

(** Magic3Example testid. *)
(* Kontrollime OCaml-i arvutustega, kas leitud väärtused tõesti moodustavad maagilise ruudu. *)
(* Kuna sobivaid väärtustusi võib üldiselt olla mitu,
   siis me ei kontrolli muutujate väärtusi (ehk väärtustust) otse,
   vaid lihtsalt kontrollime nõutud võrrandeid. *)

let test_magic3_example_sums _ =
  let open Magic3Example in
  let assert_equal = assert_equal ~printer:string_of_int in
  (* Leiame (eeldatava) maagilise konstandi esimesest reast. *)
  let magic = x11 + x12 + x13 in
  (* Kontrollime, kas kõik ülejäänud read, veerud ja diagonaalid on sama summaga. *)
  assert_equal magic (x21 + x22 + x23);
  assert_equal magic (x31 + x32 + x33);
  assert_equal magic (x11 + x21 + x31);
  assert_equal magic (x12 + x22 + x32);
  assert_equal magic (x13 + x23 + x33);
  assert_equal magic (x11 + x22 + x33);
  assert_equal magic (x13 + x22 + x31)

let test_magic3_example_nums _ =
  let open Magic3Example in
  let xs = [x11; x12; x13; x21; x22; x23; x31; x32; x33] in
  (* Kontrollime, kas ruudus esinevad kõik arvud 1-st 9-ni. *)
  assert_equal ~printer:[%show: int list] [1; 2; 3; 4; 5; 6; 7; 8; 9] (List.sort Int.compare xs)

(** Magic2Example test. *)
let test_magic2_example _ =
  (* Tõestuse korral mingit väärtustust pole, seega saame kontrollida ainult Z3 tulemust.
     Seega peame ise veenduma, et tõestasime õiget asja. *)
  assert_equal ~printer:Solver.string_of_status UNSATISFIABLE Magic2Example.status

let tests =
  "magic" >::: [
    "magic3_example" >::: [
      "sums" >:: test_magic3_example_sums;
      "nums" >:: test_magic3_example_nums;
    ];
    "magic2_example" >:: test_magic2_example;
  ]
