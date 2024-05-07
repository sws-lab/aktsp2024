(** Võrrandisüsteem üle võre. *)
module type Sys =
sig
  (** Võrrandisüsteemi muutuja moodul. *)
  module V: sig
    (** Muutuja tüüp. *)
    type t [@@deriving eq, ord, hash, show]
    (* Lisaks funktsioonid equal, compare, hash, show. *)
  end

  (** Võrrandisüsteemi muutuja väärtuste moodul.
      Siin mingi võre (domeen). *)
  module D: Domain.S

  (** Kõik võrrandisüsteemi muutujad. *)
  val vars: V.t list

  (** Võrrandisüsteemi võrrandid funktsiooni kujul.
      Esimene argument on muutuja, mille paremat poolt arvutatakse.
      Teine argument on funktsioon, mille kaudu saab küsida teiste (või sama!) muutujate väärtusi. *)
  val f: V.t -> (V.t -> D.t) -> D.t
end


