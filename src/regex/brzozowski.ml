(** Brzozowski tuletis. *)
open Re

(** Kas regulaaravaldis sobitub tühisõnega? *)
let rec matches_eps (r: t): bool =
  match r with
  | Empty -> false
  | Eps -> true
  | Char _ -> false
  | Choice (l, r) -> matches_eps l || matches_eps r
  | Concat (l, r) -> matches_eps l && matches_eps r
  | Star _ -> true

(** Regulaaravaldise tuletis antud tähe järgi.
    Antud regulaaravaldise keelest võetakse kõik sõned, mis algavad antud tähega ja eemaldatakse algusest see täht.
    Regulaaravaldise tuletis kirjeldab tulemuseks olevat keelt.

    Vt. https://en.wikipedia.org/wiki/Brzozowski_derivative.
    Vt. https://dl.acm.org/doi/10.1145/2034773.2034801.
    Vt. https://softwarefoundations.cis.upenn.edu/lf-current/IndProp.html#derive.

    Vihje: matches_eps. *)
let rec derive (r: t) (c: char): t =
  (* print_endline (show r); *)
  match r with
  | Empty -> Empty
  | Eps -> Empty
  | Char c' -> if c = c' then Eps else Empty
  | Choice (l, r) -> Choice (derive l c, derive r c)
  | Concat (l, r) ->
    let d = Concat (derive l c, r) in
    if matches_eps l then
      Choice (d, derive r c)
    else
      d
  | Star r' -> Concat (derive r' c, r)

(** Kas regulaaravaldis sobitub antud sõnega?
    Kasutada derive ja matches_eps funktsioone.
    Vihje: String.fold_left. *)
let matches (s: string) (r: t): bool =
  let r' = String.fold_left derive r s in
  matches_eps r'
