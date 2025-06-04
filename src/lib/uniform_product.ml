open! Containers

module Make
    (L : Sig.S)
    (N : sig
      val n : int
    end) =
struct
  exception Unequal_lengths

  (* type t = L.t list *)

  let bottom = List.init N.n (fun _ -> L.bottom)

  let f2 f f' l1 l2 =
    try `Ok (f f' l1 l2) with Invalid_argument _ -> `Unequal_lengths

  let map2 = f2 List.map2
  let for_all2 = f2 List.for_all2

  let join x y =
    match map2 L.join x y with
    | `Ok x -> x
    | `Unequal_lengths -> raise Unequal_lengths

  let meet x y =
    match map2 L.meet x y with
    | `Ok x -> x
    | `Unequal_lengths -> raise Unequal_lengths

  let leq x y =
    match for_all2 L.leq x y with
    | `Ok x -> x
    | `Unequal_lengths -> raise Unequal_lengths

  let to_string x =
    let l = [ "]" ] in
    let l = ("[" :: List.map L.to_string x) @ l in
    String.concat ";" l
end
