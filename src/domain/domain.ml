(** Domeenid. *)

(** Poolvõre. *)
module type Semilattice =
sig
  type t [@@deriving eq, ord, show]

  (** Osaline järjestus.

      Relatsioon, mis on:
      1. refleksiivne,
      2. antisümmeetriline,
      3. transitiivne. *)
  val leq: t -> t -> bool

  (** Ülemraja operatsioon.

      Leiab elemendi, mis on mõlemast antud elemendist suurem (leq järgi)
      ja on vähim selline element (leq järgi). *)
  val join: t -> t -> t
end

(** Tõkestatud poolvõre. *)
module type BoundedSemilattice =
sig
  include Semilattice

  (** Vähim element (leq järgi). *)
  val bot: t
end

(** Meie domeen on tõkestatud poolvõre. *)
module type S = BoundedSemilattice


(** Lisab poolvõrele tehisliku vähima elemendi,
    et saada tõkestatud poolvõre. *)
module LiftBot (D: Semilattice) =
struct
  type t =
    | Lift of D.t
    | Bot
  [@@deriving eq, ord]

  let pp ppf = function
    | Bot -> Format.pp_print_string ppf "⊥"
    | Lift x -> Format.fprintf ppf "Lift %a" D.pp x

  let show = Format.asprintf "%a" pp

  let bot = Bot
  let leq x1 x2 =
    match x1, x2 with
    | Bot, _ -> true
    | Lift _, Bot -> false
    | Lift d1, Lift d2 -> D.leq d1 d2
  let join x1 x2 =
    match x1, x2 with
    | Bot, x
    | x, Bot -> x
    | Lift d1, Lift d2 -> Lift (D.join d1 d2)
end

(** Moodustab võrreldamatutest elementidest "lameda" võre,
    lisades tehislikud vähima ja suurima elemendi. *)
module Flat (E: sig type t [@@deriving eq, ord, show] end) =
struct
  type t =
    | Top
    | Lift of E.t
    | Bot
  [@@deriving eq, ord]

  let pp ppf = function
    | Bot -> Format.pp_print_string ppf "⊥"
    | Lift x -> Format.fprintf ppf "Lift %a" E.pp x
    | Top -> Format.pp_print_string ppf "⊤"

  let show = Format.asprintf "%a" pp

  let bot = Bot
  let leq x1 x2 =
    match x1, x2 with
    | Bot, _ -> true
    | Lift _, Bot -> false
    | Lift d1, Lift d2 -> E.equal d1 d2
    | _, Top -> true
    | Top, _ -> false
  let join x1 x2 =
    match x1, x2 with
    | Bot, x
    | x, Bot -> x
    | Lift d1, Lift d2 when E.equal d1 d2 -> Lift d1
    | Lift _, Lift _ -> Top
    | Top, _
    | _, Top -> Top
end
