(** Grammatikate võrrandisüsteemid. *)

(** Grammatikate terminalsümboliteks on tähed. *)
module T =
struct
  type t = char [@@deriving ord, show]
end

(** Näited. *)

(** T → R
    T → aTc
    R →
    R → bR

    Vt. "Introduction to Compiler Design" õpikust, peatükk 2.7, grammatika 2.9. *)
module ExampleGrammar =
struct
  (** Grammatika mitteterminalideks on konstruktorid T ja R. *)
  module NT =
  struct
    type t = T | R [@@deriving eq, ord, hash, show]
  end

  open NT (* Avame NT mooduli, et mitteterminale otse kasutada. *)

  (** Nullable võrrandisüsteem. *)
  module NullableSys =
  struct
    module V = NT (** Muutujad on mitteterminalid. *)
    module D = BoolDomain (** Väärtused on tõeväärtused. *)

    let vars = [T; R] (** Kõik muutujad. *)

    (** Nullable võrrandite paremad pooled.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      match nt with
      (*  T → R
          T → aTc
          R →
          R → bR *)
      | T -> get R ||
            (false && get T && false)
      | R -> true ||
            (false && get R)
  end

  (** FIRST võrrandisüsteem. *)
  module FirstSys =
  struct
    module V = NT (** Muutujad on mitteterminalid. *)
    module D = SetDomain.Make (T) (** Väärtused on terminalide hulgad. *)

    let vars = [T; R] (** Kõik muutujad. *)

    (** FIRST võrrandite paremad pooled.
        Võid eeldada Nullable lahendit.
        Järgi täpselt grammatikat ja ära ise lihtsusta!
        Vihje: D.empty.
        Vihje: D.singleton.
        Vihje: D.join. *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      match nt with
      (*  T → R
          T → aTc
          R →
          R → bR *)
      | T -> D.join (get R) (D.singleton 'a')
      | R -> D.join D.empty (D.singleton 'b')
  end

  (** FOLLOW jaoks saaks samuti võrrandisüsteemi moodustada,
      aga me seda siin ei tee. *)
end

(** A → BAa
    A →
    B → bBc
    B → AA

    Vt. "Introduction to Compiler Design" õpikust, ülesanne 2.9. *)
module ExerciseGrammar =
struct
  module NT =
  struct
    type t = A | B [@@deriving eq, ord, hash, show]
  end

  open NT

  module NullableSys =
  struct
    module V = NT
    module D = BoolDomain

    let vars = [A; B]

    (** Nullable võrrandite paremad pooled.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      (* A → BAa
         A →
         B → bBc
         B → AA *)
      match nt with
      | A -> (get B && get A && false) || true
      | B -> (false && get B && false) || (get A && get A)
  end

  module FirstSys =
  struct
    module V = NT
    module D = SetDomain.Make (T)

    let vars = [A; B]

    (** FIRST võrrandite paremad pooled.
        Võid eeldada Nullable lahendit.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      (* A → BAa
         A →
         B → bBc
         B → AA *)
      match nt with
      | A -> D.join (D.join (get B) (D.join (get A) (D.singleton 'a'))) D.empty
      | B -> D.join (D.singleton 'b') (D.join (get A) (get A))
  end
end


(** Ülesanded. *)

(** T → R
    T → aTc
    R →
    R → RbR

    Vt. "Introduction to Compiler Design" õpikust, grammatika 2.4. *)
module Example2Grammar =
struct
  module NT =
  struct
    type t = T | R [@@deriving eq, ord, hash, show]
  end

  open NT

  module NullableSys =
  struct
    module V = NT
    module D = BoolDomain

    let vars = [T; R]

    (** Nullable võrrandite paremad pooled.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      failwith "TODO"
  end

  module FirstSys =
  struct
    module V = NT
    module D = SetDomain.Make (T)

    let vars = [T; R]

    (** FIRST võrrandite paremad pooled.
        Võid eeldada Nullable lahendit.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      failwith "TODO"
  end
end

(** N → AB
    N → BA
    A → a
    A → CAC
    B → b
    B → CBC
    C → a
    C → b

    Vt. "Introduction to Compiler Design" õpikust, peatükk 2.10. *)
module LargeGrammar =
struct
  module NT =
  struct
    type t = N | A | B | C [@@deriving eq, ord, hash, show]
  end

  open NT

  module NullableSys =
  struct
    module V = NT
    module D = BoolDomain

    let vars = [N; A; B; C]

    (** Nullable võrrandite paremad pooled.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      failwith "TODO"
  end

  module FirstSys =
  struct
    module V = NT
    module D = SetDomain.Make (T)

    let vars = [N; A; B; C]

    (** FIRST võrrandite paremad pooled.
        Võid eeldada Nullable lahendit.
        Järgi täpselt grammatikat ja ära ise lihtsusta! *)
    let f (nt: V.t) (get: V.t -> D.t): D.t =
      failwith "TODO"
  end
end


