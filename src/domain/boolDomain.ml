(** Tõeväärtuste domeen.

    false on võres väiksem kui true. *)

type t = bool [@@deriving eq, ord, show]
let bot = false
let leq x y = Crashcourse.Basics.implies x y
let join x y = x || y
