(** Abstraktne süntaksipuu. *)

(** Tüübid. *)

(** Muutuja nimi. *)
type var = string [@@deriving eq, ord]

(** Binaarne operaator. *)
type binary =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | Eq (** == *)
  | Ne (** <> *)
  | Lt (** < *)
  | Le (** <= *)
  | Gt (** > *)
  | Ge (** >= *)
[@@deriving eq, ord]

(** Avaldis. *)
type expr =
  | Num of int (** Konstant *)
  | Var of var (** Muutuja *)
  | Rand of int * int (** Juhuarv intervallist *)
  | Binary of expr * binary * expr (** Binaarne operaator *)
[@@deriving eq, ord]

(** Lause. *)
type stmt =
  | Assign of var * expr (** Omistamine *)
  | Error (** Vea tekkimine *)
  | If of expr * stmt * stmt (** Tingimuslause *)
  | While of expr * stmt (** While-tsükkel *)
  | Seq of stmt * stmt (** Järjestik-kompositsioon *)
  | Nop (** Mitte millegi tegemine *)
[@@deriving eq, ord]


(** Väljatrüki funktsioonid. *)

let pp_var ppf var = Format.pp_print_string ppf var
let show_var var = var

let pp_binary ppf binary =
  match binary with
  | Add -> Format.pp_print_string ppf "+"
  | Sub -> Format.pp_print_string ppf "-"
  | Mul -> Format.pp_print_string ppf "*"
  | Div -> Format.pp_print_string ppf "/"
  | Mod -> Format.pp_print_string ppf "%"
  | Eq -> Format.pp_print_string ppf "=="
  | Ne -> Format.pp_print_string ppf "<>"
  | Lt -> Format.pp_print_string ppf "<"
  | Le -> Format.pp_print_string ppf "<="
  | Gt -> Format.pp_print_string ppf ">"
  | Ge -> Format.pp_print_string ppf ">="

let show_binary binary = Format.asprintf "%a" pp_binary binary

let rec pp_expr ppf expr =
  match expr with
  | Num i -> Format.pp_print_int ppf i
  | Var v -> pp_var ppf v
  | Rand (l, u) -> Format.fprintf ppf "[%d, %d]" l u
  | Binary (l, b, r) -> Format.fprintf ppf "(%a %a %a)" pp_expr l pp_binary b pp_expr r

let show_expr expr = Format.asprintf "%a" pp_expr expr

let rec pp_stmt ppf stmt =
  match stmt with
  | Assign (v, e) -> Format.fprintf ppf "%a = %a" pp_var v pp_expr e
  | Error -> Format.fprintf ppf "error()"
  | If (c, t, f) -> Format.fprintf ppf "if (%a) %a else %a" pp_expr c pp_stmt t pp_stmt f
  | While (c, b) -> Format.fprintf ppf "while (%a) %a" pp_expr c pp_stmt b
  | Seq (a, b) -> Format.fprintf ppf "{%a; %a}" pp_stmt a pp_stmt b
  | Nop -> Format.pp_print_string ppf ""

let show_stmt stmt = Format.asprintf "%a" pp_stmt stmt


(** OCaml-i DSL AST-ide loomiseks. *)
module Syntax =
struct
  let (!) v = Var v
  let (~$) i = Num i
  let (+) a b = Binary (a, Add, b)
  let (-) a b = Binary (a, Sub, b)
  let ( * ) a b = Binary (a, Mul, b)
  let (/) a b = Binary (a, Div, b)
  let (mod) a b = Binary (a, Mod, b)
  let (=) a b = Binary (a, Eq, b)
  let (<>) a b = Binary (a, Ne, b)
  let (<) a b = Binary (a, Lt, b)
  let (<=) a b = Binary (a, Le, b)
  let (>) a b = Binary (a, Gt, b)
  let (>=) a b = Binary (a, Ge, b)

  let (:=) v e = Assign (v, e)
  let if_ c t f = If (c, t, f)
  let while_ c b = While (c, b)

  let assert_ e = If (e, Nop, Error)
  let assume e = If (e, Nop, While (Num 1, Nop))

  (** Järjestik-kompositsioon suvalisest arvust lausetest. *)
  let rec seq = function
    | [] -> Nop
    | [x] -> x
    | x :: xs -> Seq (x, seq xs)
end
