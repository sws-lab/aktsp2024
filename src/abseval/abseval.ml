(** Abstraktne väärtustaja ehk abstraktne interpretaator. *)
open Ast

(** Abstraktseid väärtustajaid saab luua kasutades erinevaid täisarve abstraheerivaid domeene.
    Vt. IntDomain. *)
module Make (ID: IntDomain.S) =
struct
  (** Väärtuskeskkonna domeen kasutades antud täisarvude domeeni. *)
  module ED = EnvDomain.Make (ID)

  (** Väärtustab avaldise keskkonnas.
      Vihje: ID.of_int.
      Vihje: ID.of_interval.
      Vihje: ID.eval_binary. *)
  let rec eval_expr (env: ED.t) (expr: expr): ID.t =
    failwith "TODO"

  (** Väärtustab valvuri (avaldis ja selle oodatav tõeväärtus) keskkonnas.
      Kui valvur on keskkonnaga vastuolus, siis tagastab saavutamatu programmi oleku ED.bot.
      Kui valvuriga saab keskkonna muutujate väärtusi täpsemaks kitsendada, siis võib keskkonda muuta.
      Võib jätta keskkonna muutmata, kuid siis ei kasutata valvurist saadavat lisainfot. *)
  let eval_guard (env: ED.t) (expr: expr) (branch: bool): ED.t =
    failwith "TODO"

  module EDFP = Fixpoint.MakeDomain (ED)

  (** Väärtustab lause keskkonnas.
      Vihje: Vea jaoks kasuta failwith funktsiooni.
      Vihje: eval_guard.
      Vihje: While jaoks kasuta püsipunkti moodulit EDFP. *)
  let rec eval_stmt (env: ED.t) (stmt: stmt): ED.t =
    failwith "TODO"
end
