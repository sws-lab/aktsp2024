(** Alamhulkade domeen. *)
module Make (E: sig type t [@@deriving ord, show] end) =
struct
  module S =
  struct
    include Set.Make (E)
    let pp ppf m =
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      Format.fprintf ppf "@[{%a}@]" (Format.pp_print_list ~pp_sep E.pp) (elements m)
  end

  (** Elementideks on hulgad. *)
  type t = S.t [@@deriving eq, ord, show]

  (** Vähim element on tühihulk. *)
  let bot = S.empty

  (** Osaline järjestus on hulkade sisalduvuse järgi. *)
  let leq s1 s2 = S.subset s1 s2

  (** Ülemraja on hulkade ühend. *)
  let join s1 s2 = S.union s1 s2

  (** Mõned hulkade operatsioonid. *)

  let singleton = S.singleton
  let add = S.add
  let empty = S.empty
  let fold = S.fold
  let remove = S.remove
end
