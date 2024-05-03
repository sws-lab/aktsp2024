(** Abifunktsioonid sümboolseks väärtustamiseks Z3-ga. *)
open Z3

let ctx = mk_context [
    ("model", "true");
  ]
let solver = Solver.mk_simple_solver ctx

(** OCaml-i DSL Z3 avaldiste loomiseks.
    Vt Ast.Syntax. *)
module Syntax =
struct
  (** Aritmeetilised avaldised.
      Samad, mis AST-is. *)

  let (!) v = Arithmetic.Integer.mk_const_s ctx v
  let (~$) i = Arithmetic.Integer.mk_numeral_i ctx i
  let (+) x y = Arithmetic.mk_add ctx [x; y]
  let (-) x y = Arithmetic.mk_sub ctx [x; y]
  let ( * ) x y = Arithmetic.mk_mul ctx [x; y]
  let (/) x y = Arithmetic.mk_div ctx x y
  let (mod) x y = Arithmetic.Integer.mk_mod ctx x y
  let (<) x y = Arithmetic.mk_lt ctx x y
  let (<=) x y = Arithmetic.mk_le ctx x y
  let (>) x y = Arithmetic.mk_gt ctx x y
  let (>=) x y = Arithmetic.mk_ge ctx x y
  let (=) x y = Boolean.mk_eq ctx x y
  let (<>) x y = Boolean.mk_distinct ctx [x; y]

  (** Ternary operaator.
      Seda AST-is pole. *)
  let if_ c t f = Boolean.mk_ite ctx c t f

  (** Loogilised avaldised.
      Neid AST-is pole. *)

  let true_ = Boolean.mk_true ctx
  let false_ = Boolean.mk_false ctx
  let (&&) x y = Boolean.mk_and ctx [x; y]
  let (||) x y = Boolean.mk_or ctx [x; y]
  let not x = Boolean.mk_not ctx x
  let implies x y = Boolean.mk_implies ctx x y
  let iff x y = Boolean.mk_iff ctx x y
end

(** Teisendab Z3 loogilise avaldise Z3 aritmeetiliseks avaldiseks
    kasutades meie väärtustamise semantikat.
    Vt. Eval.Concrete.int_of_bool. *)
let int_of_bool expr = Syntax.(if_ expr ~$1 ~$0)

(** Teisendab Z3 aritmeetilise avaldise Z3 loogiliseks avaldiseks
    kasutades meie väärtustamise semantikat.
    Vt. Eval.Concrete.bool_of_int. *)
let bool_of_int expr = Syntax.(expr <> ~$0)

(** Kombineerib binaarse operaatori Z3 avaldiseks.
    Vt. Eval.Concrete.eval_binary.
    Vihje: let open Syntax in ...
    Vihje: int_of_bool. *)
let eval_binary (l: Expr.expr) (b: Ast.binary) (r: Expr.expr): Expr.expr =
  let open Syntax in
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

(** Teisendab avaldise Z3 avaldiseks.
    Rand avaldist pole vaja toetada, sest see pole nii lihtne kui võib arvata.
    Vihje: let open Syntax in ...
    Vihje: eval_binary. *)
let rec eval_expr (expr: Ast.expr): Expr.expr =
  let open Syntax in
  match expr with
  | Num i -> ~$i
  | Var v -> !v
  | Rand (l, r) -> failwith "eval_expr: Rand"
  | Binary (l, b, r) -> eval_binary (eval_expr l) b (eval_expr r)


module Env = Eval.Common.Env

(** Abifunktsioon Z3 mudelist väärtuskeskkonna tegemiseks. *)
let env_of_model (model: Model.model): Env.t =
  List.fold_left (fun acc fd ->
      let v = Symbol.get_string (FuncDecl.get_name fd) in
      let e = Option.get (Model.get_const_interp model fd) in
      let i = Z.to_int (Arithmetic.Integer.get_big_int e) in
      Env.add v i acc
    ) Env.empty (Model.get_const_decls model)
