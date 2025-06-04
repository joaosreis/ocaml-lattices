open! Containers

type lower_bound = [ `LInf | `Int of int ]
type upper_bound = [ `HInf | `Int of int ]

let bound_leq x y =
  match (x, y) with
  | `LInf, _ -> true
  | _, `LInf -> false
  | _, `HInf -> true
  | `HInf, _ -> false
  | `Int x, `Int y -> x <= y

let%test _ = bound_leq `LInf `LInf
let%test _ = not (bound_leq `HInf `LInf)
let%test _ = bound_leq `LInf `HInf
let%test _ = bound_leq `HInf `HInf
let%test _ = not (bound_leq `HInf (`Int 0))
let%test _ = bound_leq (`Int 0) `HInf
let%test _ = bound_leq `LInf (`Int 0)
let%test _ = not (bound_leq (`Int 0) `LInf)
let%test _ = bound_leq (`Int 0) (`Int 1)
let%test _ = not (bound_leq (`Int 1) (`Int 0))
let%test _ = bound_leq (`Int 0) (`Int 0)

let bound_to_string = function
  | `LInf -> "-inf"
  | `HInf -> "+inf"
  | `Int x -> Int.to_string x

let lbound_min x y =
  match (x, y) with
  | `LInf, _ | _, `LInf -> `LInf
  | `Int x, `Int y -> `Int (min x y)

let lbound_max x y =
  match (x, y) with
  | `LInf, x | x, `LInf -> x
  | `Int x, `Int y -> `Int (max x y)

let ubound_min x y =
  match (x, y) with
  | `HInf, x | x, `HInf -> x
  | `Int x, `Int y -> `Int (min x y)

let ubound_max x y =
  match (x, y) with
  | `HInf, _ | _, `HInf -> `HInf
  | `Int x, `Int y -> `Int (max x y)

type t = Empty | Interval of lower_bound * upper_bound

let bottom = Empty
let top = Interval (`LInf, `HInf)

let leq x y =
  match (x, y) with
  | Empty, _ -> true
  | _, Empty -> false
  | Interval (a, b), Interval (a', b') -> bound_leq a' a && bound_leq b b'

let join x y =
  match (x, y) with
  | Empty, x | x, Empty -> x
  | Interval (a, b), Interval (a', b') ->
      Interval (lbound_min a a', ubound_max b b')

let meet x y =
  match (x, y) with
  | Empty, _ | _, Empty -> Empty
  | Interval (a, b), Interval (a', b') ->
      Interval (lbound_max a a', ubound_min b b')

let to_string = function
  | Empty -> "[]"
  | Interval (l, h) ->
      let l_string = bound_to_string l in
      let h_string = bound_to_string h in
      Printf.sprintf "[%s,%s]" l_string h_string

(* TODO implement operations *)
