open! Containers

module type S = sig
  type elt
  type t = [ `Top | `Some of elt ]

  include Sig.S with type t := t

  val top : t
end

module Make (L : Sig.S) = struct
  type elt = L.t
  type t = [ `Top | `Some of elt ]

  let bottom = `Some L.bottom
  let top = `Top

  let leq x y =
    match (x, y) with
    | _, `Top -> true
    | `Top, _ -> false
    | `Some a, `Some b -> L.leq a b

  let join x y =
    match (x, y) with
    | `Some a, `Some b -> `Some (L.join a b)
    | `Top, _ | _, `Top -> `Top

  let meet x y =
    match (x, y) with
    | `Some a, `Some b -> `Some (L.meet a b)
    | `Top, x | x, `Top -> x

  let to_string = function `Top -> "¯" | `Some x -> L.to_string x
end
