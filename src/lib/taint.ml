open! Containers

include Flat.Make (struct
  include Bool

  let to_string = function true -> "tainted" | false -> "untainted"
end)

let join_taint x y =
  let open Flat in
  match (x, y) with
  | Top, _ | _, Top -> Top
  | Element true, _ | _, Element true -> Element true
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> Element false
