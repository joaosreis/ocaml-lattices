open! Core

module Make
    (L : Sig.S) (N : sig
      val n : int
    end) =
struct
  exception Unequal_lengths

  type t = L.t list [@@deriving sexp_of]

  let bottom = List.init N.n ~f:(fun _ -> L.bottom)

  let join x y =
    match List.map2 ~f:L.join x y with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> raise Unequal_lengths

  let meet x y =
    match List.map2 ~f:L.meet x y with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> raise Unequal_lengths

  let leq x y =
    match List.for_all2 ~f:L.leq x y with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> raise Unequal_lengths

  let to_string x =
    let l = [ "]" ] in
    let l = ("[" :: List.map ~f:L.to_string x) @ l in
    String.concat ~sep:";" l
end
