(** Võrrandisüsteemi lahendajad (solverid).

    Vt. "Introduction to Compiler Design" õpikust, peatükk A.4.3.
    Vt. "Compiler Design: Analysis and Transformation" õpikust, peatükk 1.5. *)
open Sys

(** Solverid defineerime funktoritena,
    mis võtavad argumendina võrrandisüsteemi mooduli
    ning tagastavad mooduli, mille solve funktsioon lahendab võrrandisüsteemi ja tagastab lahendi. *)

(** Solverite algoritme kirjeldatakse tavaliselt imperatiivsete programmidega.
    Seega kasutame siin samuti imperatiivset OCaml-it.

    Muuhulgas kasutame funktsionaalsete mitte-modifitseeritavate Map-ide asemel siin imperatiivsed modifitseeritavaid paisktabeleid.
    OCaml-i standardteegis on need Hashtbl moodulis. *)


(** Kleene iteratsiooniga solver.
    Väärtustab võrrandeid samaaegselt kuni saavutatakse püsipunkt. *)
module Kleene (Sys: Sys) =
struct
  open Sys (* Avame võrrandisüsteemi mooduli, et oleks otsene ligipääs selle komponentidele. *)

  (* Kleene iteratsioon on lihtsalt vähima püsipunkti leidmine,
     kus domeenis on Map-ina kõigi võrrandisüsteemi muutujate väärtused. *)
  module VD = MapDomain.Make (V) (D)
  module VDFP = Fixpoint.MakeDomain (VD)

  (** Kleene iteratsiooni funktsioon.
      Väärtustab lihtsalt kõigi muutujate paremad pooled. *)
  let f' (rho: VD.t): VD.t =
    let get v = VD.find v rho in (* Abifunktsioon parema poole teiseks argumendiks. *)
    List.fold_left (fun acc v ->
        let d' = f v get in (* Väärtustame parema poole. *)
        VD.add v d' acc
      ) VD.empty vars

  (** Imperatiivsete paisktabelite moodul, kus võtmeteks on võrrandisüsteemi muutujad. *)
  module VH = Hashtbl.Make (V)

  (** Lahendab võrrandisüsteemi ja tagastab lahendi.
      Lahendiks on paisktabel, mille võtmed on võrrandisüsteemi muutujad ja väärtused on võrrandsüsteemi võre elemendid. *)
  let solve (): D.t VH.t =
    (* Kleene iteratsioon kasutades vähimat püsipunkti. *)
    let rho = VDFP.lfp f' in

    (* Teisendame funktsionaalse Map-i imperatiivseks Hashtbl-iks. *)
    let rho' = VH.create (List.length vars) in (* Paisktabeli loomise argumendiks on selle algne suurus, mis efektiivsuse mõttes on meil ette teada. *)
    List.iter (fun v ->
        let d = VD.find v rho in
        VH.replace rho' v d (* Paisktabelisse lisamiseks on replace funktsioon (mitte add!). *)
      ) vars;

    rho'
end

(** Round-robin iteratsiooniga solver.
    Väärtustab võrrandeid ükshaaval kuni saavutatakse püsipunkt. *)
module RoundRobin (Sys: Sys) =
struct
  open Sys

  module VH = Hashtbl.Make (V)

  (** Vihje: ref.
      Vihje: while ... do ...
      Vihje: VH.find. *)
  let solve (): D.t VH.t =
    failwith "TODO"
end


