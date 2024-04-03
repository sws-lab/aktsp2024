(** Väärtuskeskkonna domeen.
    Muutujate nimed on sõned ja väärtused mingist domeenist. *)
module Make (D: Domain.S) =
struct
  module M = MapDomain.Make (struct type t = string [@@deriving ord, show] end) (D)
  (* Lisame tehisliku vähima elemendi (kuigi kujutustel on juba olemas),
     millega tähistame saavutamatut programmi olekut. *)
  include Domain.LiftBot (M)

  (* Mõned keskkondade/kujutuste operatsioonid. *)

  let find x d =
    match d with
    | Bot -> D.bot
    | Lift m -> M.find x m
  let add x v d =
    match d with
    | Bot -> Bot
    | Lift m -> Lift (M.add x v m)
  let empty = Lift M.empty
  let singleton x v = Lift (M.singleton x v)
end
