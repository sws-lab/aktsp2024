(** Lihtne imperatiivne keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/ToyLangsImp. *)

(** Avaldis. *)
type expr =
  | Num of int (** Konstant *)
  | Var of char (** Muutuja *)
  | Neg of expr (** Unaarne - *)
  | Add of expr * expr (** + *)
  | Div of expr * expr (** / *)

(** Omistamine. *)
type assign = char * expr

(** Programm. *)
type prog = assign list * expr



(** Väärtustab programmi koos omistamistega.
    Vihje: Kirjuta abifunktsioonid avaldiste ja omistamiste väärtustamiseks.
    Vihje: List.fold_left. *)
let eval_prog ((assigns, expr): prog): int =
  failwith "TODO"
