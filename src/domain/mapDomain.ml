(** Kujutuste domeen. *)
module Make (K: sig type t [@@deriving ord, show] end) (D: Domain.S) =
struct
  module M =
  struct
    include Map.Make (K)
    let pp pp_v ppf m =
      let pp_kv ppf (k, v) = Format.fprintf ppf "@[%a:@ %a@]" K.pp k pp_v v in
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      Format.fprintf ppf "@[{%a}@]" (Format.pp_print_list ~pp_sep pp_kv) (bindings m)
  end

  (** Elementideks on kujutused. *)
  type t = D.t M.t [@@deriving eq, ord, show]

  (** Leiab väärtuse, mis vaikimisi on vähim element. *)
  let find k m =
    match M.find_opt k m with
    | None -> D.bot
    | Some d -> d

  (** Vähim element on tühi kujutus. *)
  let bot = M.empty

  (** Osaline järjestus on punktiviisiline. *)
  let leq m1 m2 =
    M.for_all (fun k d1 ->
        let d2 = find k m2 in
        D.leq d1 d2
      ) m1

  (** Ülemraja on punktiviisiline. *)
  let join m1 m2 =
    M.merge (fun _ d1 d2 ->
        match d1, d2 with
        | None, None -> None
        | Some d1, None -> Some d1
        | None, Some d2 -> Some d2
        | Some d1, Some d2 -> Some (D.join d1 d2)
      ) m1 m2

  (** Mõned kujutuste operatsioonid. *)

  let add = M.add
  let empty = M.empty
  let singleton = M.singleton
end
