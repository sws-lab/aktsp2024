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

let rec simplify (r: t): t =
  failwith "TODO"


(** Alt-üles lahendus.

    Lihtsustab kõigepealt puu lehti.
    Ei vaja püsipunkti.

    Vihje: Re.equal. *)

let rec simplify' (r: t): t =
  failwith "TODO"
