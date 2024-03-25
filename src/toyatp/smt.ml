(** Kehtestatavuse ülesanded koos teooriatega ehk SMT.

    SMT ülesanded on SAT ülesannete üldistused,
    kus lisaks tõeväärtustele saab kasutada ka muid andmetüüpe.
    Näiteks:
    1. täisarvud (piiramata suurusega, a la BigInt),
    2. reaalarvud (piiramata suurusega, a la BigDecimal),
    3. fikseeritud pikkused bitivektorid (ehk täisarvude piiratud suurusega esitused arvutites),
    4. IEEE ujukomaarvud (ehk reaalarvude piiratud suurusega esitused arvutites),
    5. massiivid,
    6. sõned,
    7. regulaaravaldised,
    8. jne.

    Nende erinevate andmetüüpidega arutlemiseks kasutatakse vastavaid "teooriaid",
    mistõttu SMT = SAT + teooria.
    Kuigi igasuguseid "raskeid" ülesandeid saab taandada SAT ülesanneteks,
    siis see võib olla tülikas ja ebaefektiivne.
    Täiendavate teooriate kasutamine võimaldab reaalseid ülesandeid lihtsamini spetsifitseerida ja efektiivsemalt lahendada.

    Vt. https://en.wikipedia.org/wiki/Satisfiability_modulo_theories. *)

(** Meie kasutatav Z3 solver ongi tegelikult SMT lahendaja. *)
open Z3

let ctx = mk_context [
    ("model", "true");
  ]
let solver = Solver.mk_simple_solver ctx

(** Abifunktsioon Z3 avaldise OCaml-i täisarvuks teisendamiseks.
    Vaja SatExample-is mudelist väärtuste kätte saamiseks. *)
let int_of_expr e =
  Z.to_int (Arithmetic.Integer.get_big_int e)

(** Olgu meil täisarvulised muutujad 🟢, 🟨 ja 🔺.
    Olgu meil võrrandisüsteem Facebook-ist:
    1. 🟢 + 🟢 = 10,
    2. 🟢*🟨 + 🟨 = 12,
    3. 🟢*🟨 - 🔺*🟢 = 🟢.

    Lahendada see võrrandisüsteem.
    St, leida muutujate väärtustus, mille korral kõik kolm võrrandit on tõesed.*)
module SatExample =
struct
  let (circle, square, triangle) =
    (* Loome täisarvu tüüpi Z3 muutujad. *)
    let circle = Arithmetic.Integer.mk_const_s ctx "circle" in
    let square = Arithmetic.Integer.mk_const_s ctx "square" in
    let triangle = Arithmetic.Integer.mk_const_s ctx "triangle" in

    (* Loome Z3 avaldised nende kolme võrrandi jaoks. *)
    (* Võrrandid ise on võrdused (mk_eq), mille väärtus on tõeväärtus,
       kuid võrduste pooltes on täisarvulist tüüpi avaldised moodulist Arithmetic.
       Pane tähele, et kasutame kahte uut moodulit:
       1. Arithmetic, mis sisaldab üldiseid aritmeetilisi operatsioone;
       2. Arithmetic.Integer, mis sisaldab täisarvulise aritmeetika operatsioone.
       Jaotus on selline, sest Z3 võimaldab ka töötada reaalarvudega (moodul Arithmetic.Real). *)
    let c1 = Boolean.mk_eq ctx (Arithmetic.mk_add ctx [circle; circle]) (Arithmetic.Integer.mk_numeral_i ctx 10) in
    let c2 = Boolean.mk_eq ctx (Arithmetic.mk_add ctx [Arithmetic.mk_mul ctx [circle; square]; square]) (Arithmetic.Integer.mk_numeral_i ctx 12) in
    let c3 = Boolean.mk_eq ctx (Arithmetic.mk_sub ctx [Arithmetic.mk_mul ctx [circle; square]; Arithmetic.mk_mul ctx [triangle; circle]]) circle in

    (* Käivitame Z3 solveri kolmele võrrandile vastavate avaldistega.
       Z3 lahendab SMT ülesande. *)
    let status = Solver.check solver [c1; c2; c3] in
    assert (status = SATISFIABLE); (* Siin näites peaks olema kehtestatav. *)

    let model = Option.get (Solver.get_model solver) in
    (* Õngitseme mudelist välja meie kolme Z3 muutuja väärtused OCaml-i täisarvudena. *)
    let circle' = int_of_expr (Option.get (Model.get_const_interp_e model circle)) in
    let square' = int_of_expr (Option.get (Model.get_const_interp_e model square)) in
    let triangle' = int_of_expr (Option.get (Model.get_const_interp_e model triangle)) in
    (* Tagastame testi jaoks kolm tõeväärtust. *)
    (circle', square', triangle')
end

(** Olgu meil täisarvuline muutuja x.
    Tõestada,
      kui x > 2,
      siis x * 2 > x + 2. *)
module ProofExample =
struct
  let status =
    (* Loome täisarvu tüüpi Z3 muutuja. *)
    let x = Arithmetic.Integer.mk_const_s ctx "x" in

    (* Loome Z3 avaldised implikatsiooni vasaku ja parema poole jaoks. *)
    let two = Arithmetic.Integer.mk_numeral_i ctx 2 in (* Abidefinitsioon Z3 täisarvulise konstandi 2 jaoks. *)
    let lhs = Arithmetic.mk_gt ctx x two in (* mk_gt vastab võrdlusele >. *)
    let rhs = Arithmetic.mk_gt ctx (Arithmetic.mk_mul ctx [x; two]) (Arithmetic.mk_add ctx [x; two]) in

    (* Loome Z3 avaldise implikatsiooni enda jaoks. *)
    let c = Boolean.mk_implies ctx lhs rhs in

    (* Loome Z3 avaldise implikatsiooni eituse jaoks, sest tahame tõestada. *)
    let c' = Boolean.mk_not ctx c in
    (* Ja käivitame Z3 solveri, et kontrollida, kas eitus on kehtestatav. *)
    let status = Solver.check solver [c'] in
    (* Siin näites peaks tulemus olema UNSATISFIABLE,
       mis tähendab, et implikatsioon kehtib. *)
    (* Tagastame solveri tulemuse testi jaoks.
       Kui tulemus on mitte-kehtestatav, siis Z3 mingit mudelit anda ei saa. *)
    status
end
