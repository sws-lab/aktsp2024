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
  match b with
  | Add (** + *) -> l + r
  | Sub (** - *) -> l - r
  | Mul (** * *) -> l * r
  | Div (** / *) -> l / r
  | Mod (** % *) -> l mod r
  | Eq (** == *) -> int_of_bool (l = r)
  | Ne (** <> *) -> int_of_bool (l <> r)
  | Lt (** < *) -> int_of_bool (l < r)
  | Le (** <= *) -> int_of_bool (l <= r)
  | Gt (** > *) -> int_of_bool (l > r)
  | Ge (** >= *) -> int_of_bool (l >= r)

(** Väärtustab avaldise keskkonnas ja oraakliga.
    NB! Väärtustamise järjekord on oluline.
    Vihje: eval_binary. *)
let rec eval_expr (env: env) (oracle: oracle) (expr: expr): int =
  match expr with
  | Num i -> i
  | Var v -> Env.find v env
  | Rand (l, r) -> oracle (l, r)
  | Binary (l, b, r) ->
    let i1 = eval_expr env oracle l in
    let i2 = eval_expr env oracle r in
    eval_binary i1 b i2

(** Väärtustab lause keskkonnas ja oraakliga.
    Vihje: Vea jaoks kasuta failwith funktsiooni.
    Vihje: bool_of_int.
    Vihje: While jaoks kasuta rekursiooni. *)
let rec eval_stmt (env: env) (oracle: oracle) (stmt: stmt): env =
  match stmt with
  | Nop -> env
  | Assign (v, e) ->
    let i = eval_expr env oracle e in
    Env.add v i env
  | Seq (s1, s2) ->
    let env' = eval_stmt env oracle s1 in
    eval_stmt env' oracle s2
  | If (c, t, f) ->
    if bool_of_int (eval_expr env oracle c) then
      eval_stmt env oracle t
    else
      eval_stmt env oracle f
  | While (c, b) ->
    if bool_of_int (eval_expr env oracle c) then
      let env' = eval_stmt env oracle b in
      eval_stmt env' oracle stmt
    else
      env
  | Error ->
    failwith "eval_stmt: Error"
