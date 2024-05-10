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
    match edge with
    | Nop -> env
    | Assign (v, e) ->
      let id = eval_expr env e in
      ED.add v id env
    | Error ->
      if ED.equal env ED.bot then
        ED.bot
      else
        failwith "eval_edge: Error"
    | Guard (e, b) -> eval_guard env e b


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
      let preds = Cfg.pred cfg node in
      let envs = List.map (fun (edge, prev_node) ->
          let env = get prev_node in
          eval_edge env edge
        ) preds
      in
      let env' = List.fold_left D.join D.bot envs in
      if V.equal node cfg.entry then
        D.join entry_env env'
      else
        env'
  end
end
