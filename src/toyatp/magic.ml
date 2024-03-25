(** Maagilise ruudu ülesanded.

    Failides/moodulites Sat ja Smt olid võrdlemisi lihtsad SAT ja SMT ülesanded.
    Siin on näide veidi keerulisemast ülesandest, mida saab SMT solveri abil lahendada.
    Praktikas võib olla mõistlikum spetsifiseerida ülesanne SMT-na ja lahendada see efektiivse SMT solveriga
    selle asemel, et ise üritada konkreetse ülesande jaoks välja mõelda efektiivne algoritm.

    Olgu n naturaalarv, siis n×n maagiliseks ruuduks nimetatakse ruudukujulist tabelit,
    mille kõikide ridade, veergude ja diagonaalide summa on sama (nimetatakse maagiliseks konstandiks)
    ning elementideks on paarikaupa erinevad naturaalarvud 1, 2, 3, ..., n².

    Vt. https://en.wikipedia.org/wiki/Magic_square. *)

open Z3

let ctx = mk_context [
    ("model", "true");
  ]
let solver = Solver.mk_simple_solver ctx

(** Koostada 3×3 maagiline ruut. *)
module Magic3Example =
struct
  let ((x11, x12, x13), (x21, x22, x23), (x31, x32, x33)) =
    (* Loome Z3 muutujad ruudu kõigi lahtrite jaoks.
       Neist koosnev ruut näeks välja selline:
         x11  x12  x13
         x21  x22  x23
         x31  x32  x33 *)
    let x11 = Arithmetic.Integer.mk_const_s ctx "x11" in
    let x12 = Arithmetic.Integer.mk_const_s ctx "x12" in
    let x13 = Arithmetic.Integer.mk_const_s ctx "x13" in
    let x21 = Arithmetic.Integer.mk_const_s ctx "x21" in
    let x22 = Arithmetic.Integer.mk_const_s ctx "x22" in
    let x23 = Arithmetic.Integer.mk_const_s ctx "x23" in
    let x31 = Arithmetic.Integer.mk_const_s ctx "x31" in
    let x32 = Arithmetic.Integer.mk_const_s ctx "x32" in
    let x33 = Arithmetic.Integer.mk_const_s ctx "x33" in
    (* Loome Z3 muutuja maagilise konstandi jaoks.
       See abimuutuja võimaldab lihtsamini vajalikke võrdusi kirja panna. *)
    let magic = Arithmetic.Integer.mk_const_s ctx "magic" in

    (* Loome maagilise konstandiga võrdused ridade, veergude ja diagonaalide jaoks. *)
    let c_rows = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x12; x13]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x21; x22; x23]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x31; x32; x33]) magic;
      ]
    in
    let c_cols = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x21; x31]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x12; x22; x32]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x13; x23; x33]) magic;
      ]
    in
    let c_diags = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x22; x33]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x13; x22; x31]) magic;
      ]
    in

    (* List kõigist ruudu muutujatest. *)
    let xs = [x11; x12; x13; x21; x22; x23; x31; x32; x33] in
    (* Loome Z3 avaldise, mis väljendab, et kõik ruudu muutujad on paarikaupa erinevad. *)
    let c_distinct = Boolean.mk_distinct ctx xs in

    (* Abidefinitsioonid konstantide 1 ja 9 jaoks. *)
    let one = Arithmetic.Integer.mk_numeral_i ctx 1 in
    let nine = Arithmetic.Integer.mk_numeral_i ctx 9 in
    (* Loome Z3 avaldised, mis väljendavad, et iga lahtri väärtus on intervallis [1, 9]. *)
    let c_ranges = List.map (fun x ->
        Boolean.mk_and ctx [
            Arithmetic.mk_ge ctx x one;
            Arithmetic.mk_le ctx x nine;
          ]
      ) xs
    in

    (* Tingimused c_distinct ja c_ranges koos tähendavad, et lahtrite väärtused on mingi permutatsioon arvudest 1, 2, 3, ..., 9. *)

    (* Käivitame Z3 solveri kõigi vajalike tingimustega. *)
    let cs = c_distinct :: c_ranges @ c_rows @ c_cols @ c_diags in
    let status = Solver.check solver cs in
    assert (status = SATISFIABLE); (* Siin näites peaks olema kehtestatav. *)

    let model = Option.get (Solver.get_model solver) in
    (* Õngitseme mudelist välja meie lahtrite Z3 muutujate väärtused OCaml-i täisarvudena. *)
    let x11' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x11)) in
    let x12' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x12)) in
    let x13' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x13)) in
    let x21' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x21)) in
    let x22' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x22)) in
    let x23' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x23)) in
    let x31' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x31)) in
    let x32' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x32)) in
    let x33' = Smt.int_of_expr (Option.get (Model.get_const_interp_e model x33)) in
    (* Tagastame maagilise ruudu testi jaoks. *)
    ((x11', x12', x13'), (x21', x22', x23'), (x31', x32', x33'))
end

(** Tõestada, et 2×2 maagilist ruutu pole võimalik koostada. *)
module Magic2Example =
struct
  let status =
    (* Loome Z3 muutujad ruudu kõigi lahtrite jaoks.
       Neist koosnev ruut näeks välja selline:
         x11  x12
         x21  x22 *)
    let x11 = Arithmetic.Integer.mk_const_s ctx "x11" in
    let x12 = Arithmetic.Integer.mk_const_s ctx "x12" in
    let x21 = Arithmetic.Integer.mk_const_s ctx "x21" in
    let x22 = Arithmetic.Integer.mk_const_s ctx "x22" in
    (* Loome Z3 muutuja maagilise konstandi jaoks. *)
    let magic = Arithmetic.Integer.mk_const_s ctx "magic" in

    (* Loome maagilise konstandiga võrdused ridade, veergude ja diagonaalide jaoks. *)
    let c_rows = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x12]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x21; x22]) magic;
      ]
    in
    let c_cols = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x21]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x12; x22]) magic;
      ]
    in
    let c_diags = [
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x11; x22]) magic;
        Boolean.mk_eq ctx (Arithmetic.mk_add ctx [x12; x21]) magic;
      ]
    in

    (* List kõigist ruudu muutujatest. *)
    let xs = [x11; x12; x21; x22] in
    (* Loome Z3 avaldise, mis väljendab, et kõik ruudu muutujad on paarikaupa erinevad. *)
    let c_distinct = Boolean.mk_distinct ctx xs in

    (* Abidefinitsioonid konstantide 1 ja 4 jaoks. *)
    let one = Arithmetic.Integer.mk_numeral_i ctx 1 in
    let four = Arithmetic.Integer.mk_numeral_i ctx 4 in
    (* Loome Z3 avaldised, mis väljendavad, et iga lahtri väärtus on intervallis [1, 4]. *)
    let c_ranges = List.map (fun x ->
        Boolean.mk_and ctx [
            Arithmetic.mk_ge ctx x one;
            Arithmetic.mk_le ctx x four;
          ]
      ) xs
    in

    (* Käivitame Z3 solveri kõigi vajalike tingimustega. *)
    let cs = c_distinct :: c_ranges @ c_rows @ c_cols @ c_diags in
    let status = Solver.check solver cs in
    (* Siin näites peaks tulemus olema UNSATISFIABLE,
       mis tähendab, et maagilist ruutu pole võimalik koostada. *)
    (* Tagastame solveri tulemuse testi jaoks.
       Kui tulemus on mitte-kehtestatav, siis Z3 mingit mudelit anda ei saa. *)
    status
end
