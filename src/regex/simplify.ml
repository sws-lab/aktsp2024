(** Lihtsustamine.

    Vt. https://en.wikipedia.org/wiki/Abstract_rewriting_system.
    Vt. https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting).
    Vt. "Term Rewriting and All That" õpikut.
    Vt. "Loogika: mõtlemisest tõestamiseni" õpikust (https://dspace.ut.ee/handle/10062/24397), peatükk 19. *)
open Re

(** Lihtsustusreeglid:
    1. r|r = r
    2. rε = εr = r
    3. r** = r*
    4. r*r* = r*
    5. ∅|r = r|∅ = r
    6. ∅r = r∅ = ∅
    7. ∅* = ε
    8. ε* = ε *)

(** Ülalt-alla lahendus.

    Proovib kõigepealt lihtsustada puu juurt, aga kui ei saa, siis alampuid.

    Vihje: Re.equal.
    Vihje: Fixpoint. *)

let rec simplify_step (r: t): t =
  match r with
  | Choice (r, r') when equal r r' -> r
  | Concat (r, Eps)
  | Concat (Eps, r) -> r
  | Star (Star r) -> Star r
  | Concat (Star r, Star r') when equal r r' -> Star r
  | Choice (Empty, r)
  | Choice (r, Empty) -> r
  | Concat (Empty, r)
  | Concat (r, Empty) -> Empty
  | Star Empty -> Eps
  | Star Eps -> Eps
  | Star r -> Star (simplify_step r)
  | Choice (l, r) -> Choice (simplify_step l, simplify_step r)
  | Concat (l, r) -> Concat (simplify_step l, simplify_step r)
  | Empty
  | Eps
  | Char _ -> r

module RegexFP = Fixpoint.Make (Re)

let simplify = RegexFP.fp simplify_step


(** Alt-üles lahendus.

    Lihtsustab kõigepealt puu lehti.
    Ei vaja püsipunkti.

    Vihje: Re.equal. *)

let rec simplify'_one (r: t): t =
  match r with
  | Choice (r, r') when equal r r' -> r
  | Concat (r, Eps)
  | Concat (Eps, r) -> r
  | Star (Star r) -> Star r
  | Concat (Star r, Star r') when equal r r' -> Star r
  | Choice (Empty, r)
  | Choice (r, Empty) -> r
  | Concat (Empty, r)
  | Concat (r, Empty) -> Empty
  | Star Empty -> Eps
  | Star Eps -> Eps
  | _ -> r

let rec simplify' (r: t): t =
  match r with
  | Eps
  | Empty
  | Char _ -> r
  | Choice (l, r) -> simplify'_one (Choice (simplify' l, simplify' r))
  | Concat (l, r) -> simplify'_one (Concat (simplify' l, simplify' r))
  | Star r -> simplify'_one (Star (simplify' r))
