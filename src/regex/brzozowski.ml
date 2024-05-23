(** Brzozowski tuletis. *)
open Re

(** Kas regulaaravaldis sobitub tühisõnega? *)
let rec matches_eps (r: t): bool =
  failwith "TODO"

(** Regulaaravaldise tuletis antud tähe järgi.
    Antud regulaaravaldise keelest võetakse kõik sõned, mis algavad antud tähega ja eemaldatakse algusest see täht.
    Regulaaravaldise tuletis kirjeldab tulemuseks olevat keelt.

    Vt. https://en.wikipedia.org/wiki/Brzozowski_derivative.
    Vt. https://dl.acm.org/doi/10.1145/2034773.2034801.
    Vt. https://softwarefoundations.cis.upenn.edu/lf-current/IndProp.html#derive.

    Vihje: matches_eps. *)
let rec derive (r: t) (c: char): t =
  failwith "TODO"

(** Kas regulaaravaldis sobitub antud sõnega?
    Kasutada derive ja matches_eps funktsioone.
    Vihje: String.fold_left. *)
let matches (s: string) (r: t): bool =
  failwith "TODO"
