open! Core
open QCheck

module L = Flat_test.Make (struct
  type t = Lattices.Sign.sign [@@deriving sexp_of]

  let gen = Gen.oneofl [ Lattices.Sign.Neg; Pos; Zero ]

  let to_string = function Lattices.Sign.Zero -> "0" | Pos -> "+" | Neg -> "-"

  let equal x y =
    match (x, y) with
    | Lattices.Sign.Zero, Lattices.Sign.Zero -> true
    | Pos, Pos -> true
    | Neg, Neg -> true
    | Neg, (Zero | Pos) | Pos, (Zero | Neg) | Zero, (Pos | Neg) -> false

  let name = "sign"
end)

module LTests = LCheck.GenericTests (L)
module LTestsTop = LCheck.GenericTopTests (L)

let () =
  Alcotest.run "sign lattice"
    [
      ("properties", List.map ~f:QCheck_alcotest.to_alcotest LTests.suite);
      ("top properties", List.map ~f:QCheck_alcotest.to_alcotest LTestsTop.suite);
    ]
