open! Core

module Make (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom_elems : Set.M(D).t
end)
(L : Sig.S) =
struct
  exception Incompatible_arguments of string

  type t = L.t Map.M(D).t [@@deriving sexp_of]

  let bottom =
    Set.fold
      ~f:(fun map x -> Map.set map ~key:x ~data:L.bottom)
      B.bottom_elems
      ~init:(Map.empty (module D))

  let join x y =
    Map.mapi x ~f:(fun ~key ~data ->
        match Map.find y key with
        | Some data_y -> L.join data data_y
        | None -> raise (Incompatible_arguments "different key set"))

  let meet x y =
    Map.mapi x ~f:(fun ~key ~data ->
        match Map.find y key with
        | Some data_y -> L.meet data data_y
        | None -> raise (Incompatible_arguments "different key set"))

  let leq x y =
    if Map.length x > Map.length y then false
    else
      Map.for_alli x ~f:(fun ~key ~data ->
          match Map.find y key with
          | Some data_y -> L.leq data data_y
          | None -> raise (Incompatible_arguments "different key set"))

  let to_string x =
    let f (key, data) = [%string "%{key#D}: %{data#L}; "] in
    let s =
      List.fold_left (Map.to_alist x) ~f:(fun acc x -> acc ^ f x) ~init:""
    in
    [%string "[ %{s} ]"]

  let set x key data = Map.set x ~key ~data

  let get x key = Map.find x key
end
