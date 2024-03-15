(** Tõeväärtusavaldiste keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/Alusosa#bool. *)

type t =
  | Var of char (** Muutuja *)
  | Not of t (* not *)
  | Or of t * t (* || *)
  | Imp of t * t (** Implikatsioon *)

(** Väärtuskeskkonnaks on hulk tõestest muutujatest. *)
module CharSet = Set.Make (Char)
type env = CharSet.t


(** Vihje: Kasuta Crashcourse.Basics.implies funktsiooni. *)
let rec eval (env: env) (e: t): bool =
  failwith "TODO"
