(** Abstraktne juhtvoograafi tipp. *)
type t [@@deriving eq, ord, hash, show]

(** Loob uue unikaalse tipu. *)
val fresh: unit -> t
