(** Konkreetne väärtustaja. *)
open Ast
open Common

(** Rand avaldiste konkreetseks väärtustamiseks kasutame oraaklit.
    Vt. Münt Toylangs.Rnd-is. *)
type oracle = int * int -> int

(** Teisendab tõeväärtuse täisarvuks. *)
let int_of_bool b = if b then 1 else 0

(** Teisendab täisarvu tõeväärtuseks. *)
let bool_of_int i = i <> 0


(** Väärtustab binaarse operaatori.
    Vihje: mod operaator.
    Vihje: int_of_bool. *)
let eval_binary (l: int) (b: binary) (r: int): int =
  failwith "TODO"

(** Väärtustab avaldise keskkonnas ja oraakliga.
    NB! Väärtustamise järjekord on oluline.
    Vihje: eval_binary. *)
let rec eval_expr (env: env) (oracle: oracle) (expr: expr): int =
  failwith "TODO"

(** Väärtustab lause keskkonnas ja oraakliga.
    Vihje: Vea jaoks kasuta failwith funktsiooni.
    Vihje: bool_of_int.
    Vihje: While jaoks kasuta rekursiooni. *)
let rec eval_stmt (env: env) (oracle: oracle) (stmt: stmt): env =
  failwith "TODO"
