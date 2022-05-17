open! Core

module Make (L1 : Sig.S) (L2 : Sig.S) = struct
  type t = L1.t * L2.t [@@deriving sexp_of]

  let bottom = (L1.bottom, L2.bottom)

  let join x y = (L1.join (fst x) (fst y), L2.join (snd x) (snd y))

  let meet x y = (L1.meet (fst x) (fst y), L2.meet (snd x) (snd y))

  let leq (x_1, y_1) (x_2, y_2) =
    let b = L1.leq x_1 x_2 in
    if b && L1.leq x_2 x_1 then L2.leq y_1 y_2 else b

  let to_string x =
    Printf.sprintf "%s, %s" (fst x |> L1.to_string) (snd x |> L2.to_string)

  let fst = fst

  let snd = snd

  let of_pair x = x
end
