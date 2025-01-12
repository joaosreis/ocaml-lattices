open! Core

module Make (L : Lattices.Sig.S) = struct
  include L

  type elem = L.t

  let bot = L.bottom
  let eq x y = L.leq x y && L.leq y x
  let pprint x = print_string (L.to_string x)
end
