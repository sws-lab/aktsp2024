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

module Env = Map.Make (Char)
type env = int Env.t

let rec eval_expr (env: env) (expr: expr): int =
  match expr with
  | Num i -> i
  | Var v -> Env.find v env
  | Neg e -> -(eval_expr env e)
  | Add (e1, e2) -> eval_expr env e1 + eval_expr env e2
  | Div (e1, e2) -> eval_expr env e1 / eval_expr env e2

let eval_assign (env: env) ((v, expr): assign): env =
  let i = eval_expr env expr in
  Env.add v i env

(** Väärtustab programmi koos omistamistega.
    Vihje: Kirjuta abifunktsioonid avaldiste ja omistamiste väärtustamiseks.
    Vihje: List.fold_left. *)
let eval_prog ((assigns, expr): prog): int =
  let env = List.fold_left eval_assign Env.empty assigns in
  eval_expr env expr
