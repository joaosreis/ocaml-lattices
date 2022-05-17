open! Core

module Make (L : Sig.S) = struct
  type t = L.t list [@@deriving sexp_of]

  let bottom : t = []

  let rec leq l_1 l_2 =
    match (l_1, l_2) with
    | [], _ -> true
    | _, [] -> false
    | h_1 :: t_1, h_2 :: t_2 -> L.leq h_1 h_2 && leq t_1 t_2

  let rec join l_1 l_2 =
    match (l_1, l_2) with
    | [], _ -> l_2
    | _, [] -> l_1
    | h_1 :: t_1, h_2 :: t_2 -> L.join h_1 h_2 :: join t_1 t_2

  let rec meet l_1 l_2 =
    match (l_1, l_2) with
    | [], _ -> []
    | _, [] -> []
    | h_1 :: t_1, h_2 :: t_2 -> L.meet h_1 h_2 :: meet t_1 t_2

  let to_string x = List.map x ~f:L.to_string |> String.concat ~sep:", "
end
