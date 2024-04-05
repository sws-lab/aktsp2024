(** Bool keele kehtestatavuse kontrollija.
    Vt. Toylangs.Bool. *)
open Z3
open Toylangs.Bool

let ctx = mk_context [
    ("model", "true");
  ]
let solver = Solver.mk_simple_solver ctx


(** Abifunktsioon Bool keele muutujast Z3 muutuja nime tegemiseks. *)
let string_of_char (c: char): string =
  String.make 1 c

(** Abifunktsioon Z3 muutuja nimest Bool keele muutuja tegemiseks. *)
let char_of_string (s: string): char =
  assert (String.length s = 1);
  s.[0]


(** Teisendab Bool keele avaldise Z3 avaldiseks.
    Vihje: string_of_char. *)
let rec eval_expr (e: t): Expr.expr =
  failwith "TODO"


(** Kehtestatavuse kontrolli tulemus. *)
type verdict =
  | Satisfiable of env (** Kehtestatav. Andmetena kaasas keskkond, milles Bool keele avaldis on tõene. *)
  | Unsatisfiable (** Mitte-kehtestatav. *)
  | Unknown (** Z3 ei oska lahendada. *)

(** Abifunktsioon Z3 mudelist Bool keele keskkonna tegemiseks. *)
let env_of_model (model: Model.model): env =
  List.fold_left (fun acc fd ->
      let x = char_of_string (Symbol.get_string (FuncDecl.get_name fd)) in
      let e = Option.get (Model.get_const_interp model fd) in
      let b = Sat.bool_of_expr e in
      if b then
        CharSet.add x acc (* kui tõene, siis lisatakse keskkonna hulka *)
      else
        acc (* kui väär, siis ei lisata keskkonna hulka *)
    ) CharSet.empty (Model.get_const_decls model)

(** Kontrollib, kas Bool keele avaldis on kehtestatav.
    Vihje: eval_expr.
    Vihje: match-i Solver.check tulemust.
    Vihje: env_of_model. *)
let check (e: t): verdict =
  failwith "TODO"
