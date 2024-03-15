(** Aritmeetiliste avaldiste keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/Alusosa#expr. *)

type t =
  | Num of int (** Konstant *)
  | Neg of t (** Unaarne - *)
  | Add of t * t (** + *)
  | Div of t * t (** / *)


let rec eval (e: t): int =
  failwith "TODO"
