(** Võrrandisüsteemi kaudu abstraktne interpretatsioon. *)

(** Võrrandisüsteeme saab luua kasutades erinevaid täisarve abstraheerivad domeene.
    Vt. Abseval. *)
module Make (ID: IntDomain.S) =
struct
  (** Abstraktne interpretaator.
      Abseval.eval_stmt me siin tegelikult ei kasuta. *)
  module Abseval = Abseval.Make (ID)
  module ED = Abseval.ED
  open Abseval

  (** Väärtustab juhtvoograafi serva keskkonnas.
      Vihje: Abseval.eval_expr.
      Vihje: Abseval.eval_guard. *)
  let eval_edge (env: ED.t) (edge: Cfg.Edge.t): ED.t =
    failwith "TODO"


  (** Võrrandisüsteem juhtvoograafiga defineeritud programmi jaoks. *)
  module MakeSys (C: sig
      val cfg: Cfg.t (** Juhtvoograaf. *)

      val entry_env: ED.t (** Algne abstraktne väärtuskeskkond. *)
    end) =
  struct
    open C

    module V = Cfg.Node (** Muutujad on juhtvoograafi tipud. *)

    module D = ED (** Väärtused on väärtuskeskkonna domeenist. *)

    let vars = Cfg.nodes cfg (** Kõik juhtvoograafi tipud. *)

    (** Abstraktse väärtustamise võrrandite paremad pooled.
        Vihje: Cfg.pred.
        Vihje: eval_edge. *)
    let f (node: V.t) (get: V.t -> D.t): D.t =
      failwith "TODO"
  end
end
