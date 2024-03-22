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
  (* let eval' e' = eval env e' in *)
  let eval' = eval env in
  match e with
  | Var v -> CharSet.mem v env
  | Not e -> not (eval' e)
  | Or (e1, e2) -> eval' e1 || eval' e2
  | Imp (e1, e2) ->
    Crashcourse.Basics.implies (eval' e1) (eval' e2)
(* let eval (env: env) (e: t): bool =
  let rec eval' (e: t): bool =
    match e with
    | Var v -> CharSet.mem v env
    | Not e -> not (eval' e)
    | Or (e1, e2) -> eval' e1 || eval' e2
    | Imp (e1, e2) ->
      Crashcourse.Basics.implies (eval' e1) (eval' e2)
  in
  eval' e *)
