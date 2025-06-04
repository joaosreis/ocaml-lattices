open! Containers

module Make
    (D : sig
      type t

      module Map : Map.S with type key := t
      module Set : Set.S with type elt := t

      val to_string : t -> string
    end)
    (B : sig
      val bottom_elems : D.Set.t
    end)
    (L : Sig.S) =
struct
  exception Incompatible_arguments of string

  type t = L.t D.Map.t

  let bottom =
    D.Set.fold
      (fun x map -> D.Map.add x L.bottom map)
      B.bottom_elems D.Map.empty

  let join x y =
    D.Map.mapi
      (fun key data ->
        match D.Map.find_opt key y with
        | Some data_y -> L.join data data_y
        | None -> raise (Incompatible_arguments "different key set"))
      x

  let meet x y =
    D.Map.mapi
      (fun key data ->
        match D.Map.find_opt key y with
        | Some data_y -> L.meet data data_y
        | None -> raise (Incompatible_arguments "different key set"))
      x

  let leq x y =
    if D.Map.cardinal x > D.Map.cardinal y then false
    else
      D.Map.for_all
        (fun key data ->
          match D.Map.find_opt key y with
          | Some data_y -> L.leq data data_y
          | None -> raise (Incompatible_arguments "different key set"))
        x

  let to_string x =
    let f (key, data) = [%string "%{key#D}: %{data#L}; "] in
    let s = List.fold_left (fun acc x -> acc ^ f x) "" (D.Map.to_list x) in
    [%string "[ %{s} ]"]

  let set x key data = D.Map.add key data x
  let get x key = D.Map.find_opt key x
end
