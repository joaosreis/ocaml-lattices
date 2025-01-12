open! Core
open QCheck

module Make (L : sig
  include Lattices.Sig.S

  val gen : t Gen.t
  val name : string
end) : LCheck.LATTICE = struct
  open Lattices
  module L_1 = With_top.Make (L)
  include L_1
  include Lcheck_helper.Make (L_1)

  let equal x y = leq x y && leq y x
  let name = L.name ^ " lattice"

  let arb_elem =
    let mk_element x = `Some x in
    let gen =
      Gen.(
        frequency
          [ (1, return bottom); (1, return top); (2, map mk_element L.gen) ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      match e with
      | e when equal e bottom -> Gen.return bottom
      | e' ->
          Gen.(arb_elem.gen >>= fun x -> if leq x e' then return x else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module L = Make (Int_test)
module LTests = LCheck.GenericTests (L)
module LTestsTop = LCheck.GenericTopTests (L)

let () =
  Alcotest.run "lattice with top"
    [
      ("properties", List.map ~f:QCheck_alcotest.to_alcotest LTests.suite);
      ("top properties", List.map ~f:QCheck_alcotest.to_alcotest LTestsTop.suite);
    ]
