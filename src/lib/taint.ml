open! Core
include Flat.Make (Bool)

let join_taint x y =
  let open Flat in
  match (x, y) with
  | Top, _ | _, Top -> Top
  | Element true, _ | _, Element true -> Element true
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> Element false
