open! Core
open QCheck

module L = Flat_test.Make (struct
  include Bool

  let gen = Gen.bool
  let name = "taint"
end)

module LTests = LCheck.GenericTests (L)
module LTestsTop = LCheck.GenericTopTests (L)

let () =
  Alcotest.run "taint lattice"
    [
      ("properties", List.map ~f:QCheck_alcotest.to_alcotest LTests.suite);
      ("top properties", List.map ~f:QCheck_alcotest.to_alcotest LTestsTop.suite);
    ]
