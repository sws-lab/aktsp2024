(** Juhtvoograafi serv. *)
open Ast

type t =
  | Assign of var * expr (** Omistamine *)
  | Guard of expr * bool (** Valvur tõese/väära haru jaoks *)
  | Error (** Vea tekkimine *)
  | Nop (** Mitte millegi tegemine *)
[@@deriving ord]


(** Väljatrüki funktsioonid. *)

let pp ppf edge =
  match edge with
  | Assign (v, e) -> Format.fprintf ppf "%a = %a" pp_var v pp_expr e
  | Guard (e, true) -> Format.fprintf ppf "[%a]" pp_expr e
  | Guard (e, false) -> Format.fprintf ppf "[!(%a)]" pp_expr e
  | Error -> Format.pp_print_string ppf "error()"
  | Nop -> Format.pp_print_string ppf ""

let show edge = Format.asprintf "%a" pp edge


(** ocamlgraph-i jaoks. *)
let default = Nop
