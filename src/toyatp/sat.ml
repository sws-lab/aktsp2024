(** Lausearvutuse kehtestatavuse ülesanded ehk SAT.

    SAT ülesanded on arvutiteaduses olulisel kohal,
    sest nende lahendamine on NP-täielik.
    St, SAT ülesannete lahendamine on "raske" ja
    teised "rasked" arvutiteaduse ülesanded on taandatavad SAT ülesande lahendamisele.

    Vt. https://en.wikipedia.org/wiki/Boolean_satisfiability_problem. *)

(** Vaatamata ülesannete keerukusele on praktikas olemas väga efektiivsed SAT lahendajad (solverid).
    Me kasutame siin populaarset Z3 solverit.
    Vt. https://github.com/Z3Prover/z3/wiki. *)

(** Z3 mooduli avamine võimaldab kasutada tema alammooduleid (Boolean, Solver, ...) ilma,
    et iga kord Z3 ette kirjutada.  *)
open Z3

(** Z3 solveri kasutamiseks on vaja luua kontekst. *)
let ctx = mk_context [
    ("model", "true"); (* Z3 seadistus mudelite loomiseks, vt SatExample. *)
  ]
let solver = Solver.mk_simple_solver ctx

(** Abifunktsioon Z3 avaldise OCaml-i tõeväärtuseks teisendamiseks.
    Vaja SatExample-is mudelist väärtuste kätte saamiseks. *)
let bool_of_expr e =
  match Boolean.get_bool_value e with
  | L_TRUE -> true
  | L_FALSE -> false
  | L_UNDEF -> failwith "bool_of_expr: L_UNDEF"

(** Olgu meil lausearvutuse muutujad P, Q ja R.
    Olgu meil lausearvutuse valemid:
    1. P → Q,
    2. R ↔ ¬Q,
    3. ¬P ∨ R.

    Kas need kolm valemit on samaaegselt kehtestatavad?
    St, kas leidub muutujate väärtustus, mille korral kõik kolm valemit on tõesed?
    Kui, siis milline muutujate väärtustus seda näitab? *)
module SatExample =
struct
  let (p, q, r) =
    (* Loome tõeväärtuse tüüpi Z3 muutujad. *)
    let p = Boolean.mk_const_s ctx "p" in
    let q = Boolean.mk_const_s ctx "q" in
    let r = Boolean.mk_const_s ctx "r" in

    (* Loome Z3 avaldised nende kolme valemi jaoks. *)
    (* Boolean moodulis mk_ algusega funktsioonid, millega Z3 avaldispuu tippe saab luua.
       Kuna Z3 on C++ teek, siis tema avaldispuid ei saa mugavalt OCaml-i konstruktoritega luua.
       Samuti peab iga tipu loomisel andma kõigepealt ctx argumendi. *)
    let c1 = Boolean.mk_implies ctx p q in
    let c2 = Boolean.mk_iff ctx r (Boolean.mk_not ctx q) in
    (* Disjunktsiooni loomise funktsioon mk_or ei võta kahte argumenti,
       vaid listi disjunktidest, sest neid saab olla ka rohkem. *)
    let c3 = Boolean.mk_or ctx [(Boolean.mk_not ctx p); r] in

    (* Käivitame Z3 solveri kolmele valemile vastavate avaldistega.
       Z3 lahendab SAT ülesande, mis ongi vastus meie küsimusele,
       kas need kolm valemit on samaaegselt kehtestatavad. *)
    let status = Solver.check solver [c1; c2; c3] in
    (* Solveri tulemus saab olla:
       1. SATISFIABLE (kehtestatav),
       2. UNSATISFIABLE (mitte-kehtestatav ehk samaselt väär) või
       3. UNKNOWN (kui millegipärast peaks ülesanne olema Z3 jaoks liiga keeruline). *)
    assert (status = SATISFIABLE); (* Siin näites peaks olema kehtestatav. *)

    (* Kui valemid on kehtestatavad, siis Z3 annab meile vastava väärtustuse,
       mida Z3 kutsub mudeliks.
       Mudel antakse viimase Solver.check-i kohta, sest Z3 solveril on oma sisemine olek. *)
    let model = Option.get (Solver.get_model solver) in
    (* Õngitseme mudelist välja meie kolme Z3 muutuja väärtused OCaml-i tõeväärtustena. *)
    let p' = bool_of_expr (Option.get (Model.get_const_interp_e model p)) in
    let q' = bool_of_expr (Option.get (Model.get_const_interp_e model q)) in
    let r' = bool_of_expr (Option.get (Model.get_const_interp_e model r)) in
    (* Tagastame testi jaoks kolm tõeväärtust. *)
    (p', q', r')
end

(** Olgu meil lausearvutuse muutujad P ja Q.
    Kas kehtib samaväärsus
      P ∧ Q ≡ ¬(¬P ∨ ¬Q)
    ?

    Siin ei huvita meid, kas leiduvad muutujate väärtused, mille korral samaväärsuse pooled on samad,
    vaid hoopis, kas kõigi võimalike väärtustuste korral on samaväärsuse pooled samad.
    Kuigi Z3 lahendab kehtestatavuse esimest ülesannet,
    siis saab seda tegelikult kasutada ka teise tõestamisülesande lahendamiseks. *)
module ProofExample =
struct
  let status =
    (* Loome tõeväärtuse tüüpi Z3 muutujad. *)
    let p = Boolean.mk_const_s ctx "p" in
    let q = Boolean.mk_const_s ctx "q" in

    (* Loome Z3 avaldised samaväärsuse vasaku ja parema poole jaoks. *)
    (* Konjunktsiooni loomise funktsioon mk_and on samuti listi argumendiga. *)
    let lhs = Boolean.mk_and ctx [p; q] in
    let rhs = Boolean.mk_not ctx (Boolean.mk_or ctx [Boolean.mk_not ctx p; Boolean.mk_not ctx q]) in

    (* Loome Z3 avaldise samaväärsuse enda jaoks.
       Siin võib võrduse loomise funktsiooni mk_eq asemel kasutada ka mk_iff. *)
    let c = Boolean.mk_eq ctx lhs rhs in

    (* Lausearvutuses kehtib fakt:
       valem on samaselt tõene parajasti siis, kui tema eitus ei ole kehtestatav.
       St, kui ei leidu väärtustust samaväärsuse ümberlükkamiseks, siis ta peab kehtima. *)
    (* Seega loome Z3 avaldise samaväärsuse eituse jaoks. *)
    let c' = Boolean.mk_not ctx c in
    (* Ja käivitame Z3 solveri, et kontrollida, kas eitus on kehtestatav. *)
    let status = Solver.check solver [c'] in
    (* Siin näites peaks tulemus olema UNSATISFIABLE,
       mis tähendab, et samaväärsus kehtib. *)
    (* Tagastame solveri tulemuse testi jaoks.
       Kui tulemus on mitte-kehtestatav, siis Z3 mingit mudelit anda ei saa. *)
    status
end
