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
  match e with
  | Num i -> i
  | Neg e -> -(eval coin e)
  | Add (e1, e2) ->
    let i1 = eval coin e1 in
    let i2 = eval coin e2 in
    i1 + i2
  | Flip (e1, e2) ->
    (* if coin () then
      eval coin e1
    else
      eval coin e2 *)
    let e = if coin () then e1 else e2 in
    eval coin e


(** Konstrueerib kahe listi otsekorrutise. *)
let cartesian_product (l1: 'a list) (l2: 'b list): ('a * 'b) list =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) l2) l1

(** Leiab avaldise kõik võimalikud väärtused listina.
    NB! Tulemuste järjekord on oluline.
    Vihje: List.map.
    Vihje: cartesian_product. *)
let rec eval_list (e: t): int list =
  match e with
  | Num i -> [i]
  | Neg e -> List.map (fun i -> -i) (eval_list e)
  | Flip (e1, e2) -> eval_list e1 @ eval_list e2
  | Add (e1, e2) ->
    let is1 = eval_list e1 in
    let is2 = eval_list e2 in
    List.map (fun (a, b) -> a + b) (cartesian_product is1 is2)


module IntSet = Set.Make (Int)

(** Leiab avaldise kõik võimalikud väärtused hulgana.
    Mitte kasutada eval_list funktsiooni.
    Vihje: IntSet.map.
    Vihje: Hulkade otsekorrutise asemel võib:
           1. teisendada hulga listiks (IntSet.elements),
           2. teostada otsekorrutisega operatsioon listidel (cartesian_product),
           3. teisendada list hulgaks (IntSet.of_list). *)
let rec eval_set (e: t): IntSet.t =
  match e with
  | Num i -> IntSet.singleton i
  | Neg e -> IntSet.map (fun i -> -i) (eval_set e)
  | Flip (e1, e2) -> IntSet.union (eval_set e1) (eval_set e2)
  | Add (e1, e2) ->
    let is1 = IntSet.elements (eval_set e1) in
    let is2 = IntSet.elements (eval_set e2) in
    let is' = List.map (fun (a, b) -> a + b) (cartesian_product is1 is2) in
    IntSet.of_list is'
