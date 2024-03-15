(** Mündiviskega aritmeetiliste avaldiste keel.
    Vt. https://courses.cs.ut.ee/t/akt/Main/Alusosa#rnd. *)

type t =
  | Num of int (** Konstant *)
  | Neg of t (** Unaarne - *)
  | Add of t * t (** + *)
  | Flip of t * t (** Mündivise: juhuslik valik alamavaldiste vahel *)

(** Münt on tõeväärtusi andev funktsioon. *)
type coin = unit -> bool


(** Väärtustab avaldise etteantud mündiga.
    NB! Väärtustamise järjekord on oluline. *)
let rec eval (coin: coin) (e: t): int =
  failwith "TODO"


(** Konstrueerib kahe listi otsekorrutise. *)
let cartesian_product (l1: 'a list) (l2: 'b list): ('a * 'b) list =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1

(** Leiab avaldise kõik võimalikud väärtused listina.
    NB! Tulemuste järjekord on oluline.
    Vihje: List.map.
    Vihje: cartesian_product. *)
let rec eval_list (e: t): int list =
  failwith "TODO"


module IntSet = Set.Make (Int)

(** Leiab avaldise kõik võimalikud väärtused hulgana.
    Mitte kasutada eval_list funktsiooni.
    Vihje: IntSet.map.
    Vihje: Hulkade otsekorrutise asemel võib:
           1. teisendada hulga listiks (IntSet.elements),
           2. teostada otsekorrutisega operatsioon listidel (cartesian_product),
           3. teisendada list hulgaks (IntSet.of_list). *)
let rec eval_set (e: t): IntSet.t =
  failwith "TODO"
