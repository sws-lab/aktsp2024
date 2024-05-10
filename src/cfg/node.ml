type t = int [@@deriving eq, ord, hash, show]

let fresh =
  let i = ref 0 in
  fun () ->
    incr i;
    !i
