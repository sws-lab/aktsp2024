(** Väärtuskeskkond.
    Muutujate nimed on sõned ja väärtused täisarvud. *)
module Env =
struct
  module M = Map.Make (String)

  include M
  type t = int M.t [@@deriving eq, ord]

  let pp ppf env =
    let pp_pair ppf (x, i) = Format.fprintf ppf "%s→%d" x i in
    let pp_sep ppf () = Format.fprintf ppf ", " in
    Format.fprintf ppf "{%a}" (Format.pp_print_list ~pp_sep pp_pair) (bindings env)

  let show env = Format.asprintf "%a" pp env
end

type env = Env.t [@@deriving ord, show]
