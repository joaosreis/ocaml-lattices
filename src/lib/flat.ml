open! Core

type 'a flat = Bottom | Top | Element of 'a [@@deriving sexp_of]

module Make (X : sig
  type t [@@deriving sexp_of]

  val to_string : t -> string

  val equal : t -> t -> bool
end) =
struct
  type t = X.t flat [@@deriving sexp_of]

  let bottom = Bottom

  let top = Top

  let leq x y =
    match (x, y) with
    | Bottom, _ -> true
    | _, Bottom -> false
    | Top, Top -> true
    | Top, _ -> false
    | _, Top -> true
    | Element a, Element b -> if X.equal a b then true else false

  let equal x y =
    match (x, y) with
    | Bottom, Bottom | Top, Top -> true
    | Element x, Element y -> X.equal x y
    | Top, (Bottom | Element _)
    | Bottom, (Top | Element _)
    | Element _, (Bottom | Top) ->
        false

  let join x y =
    match (x, y) with
    | _ when equal x y -> y
    | Bottom, _ | _, Top -> y
    | _, Bottom | Top, _ -> x
    | Element _, Element _ -> Top

  let meet x y =
    match (x, y) with
    | _ when equal x y -> y
    | Top, x | x, Top -> x
    | Bottom, _ | _, Bottom -> Bottom
    | Element a, Element b when X.equal a b -> x
    | Element _, Element _ -> Bottom

  let to_string = function
    | Bottom -> "bot"
    | Top -> "top"
    | Element x -> X.to_string x
end
