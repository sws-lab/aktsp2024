(** Regulaaravaldise puu. *)
type t =
  | Empty (** Tühi keel *)
  | Eps (** Tühisõne keel *)
  | Char of char (** Ühetähelise sõne keel *)
  | Choice of t * t (** Valik *)
  | Concat of t * t (** Konkatenatsioon *)
  | Star of t (** Kleene'i tärn *)
[@@deriving eq, ord]


(** Väljatrüki funktsioonid. *)

let rec pp ppf = function
  | Empty -> Format.pp_print_string ppf "∅"
  | Eps -> Format.pp_print_string ppf "ε"
  | Char c -> Format.pp_print_char ppf c
  | Choice (l, r) -> Format.fprintf ppf "(%a|%a)" pp l pp r
  | Concat (l, r) -> Format.fprintf ppf "%a%a" pp l pp r
  | Star r -> Format.fprintf ppf "(%a)*" pp r

let show = Format.asprintf "%a" pp
