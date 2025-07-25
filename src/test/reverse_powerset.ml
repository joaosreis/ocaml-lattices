open! Containers
open Lattices
open QCheck

module Bool = struct
  include Bool
  module Set = Set.Make (Bool)

  let to_string = function true -> "true" | false -> "false"
end

module L : LCheck.LATTICE = struct
  module L_1 =
    Powerset.Make_reverse
      (Bool)
      (struct
        let bottom = Bool.Set.add true (Bool.Set.singleton false)
      end)

  include L_1
  include Lcheck_helper.Make (L_1)

  let bot = bottom
  let equal x y = leq x y && leq y x
  let name = "powerset lattice"

  let arb_elem =
    let gen =
      Gen.(
        frequency
          [
            (1, return L_1.Elt.Set.empty);
            (1, return (L_1.Elt.Set.singleton false));
            (1, return (L_1.Elt.Set.singleton true));
            (1, return bottom);
          ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let gen =
      match e with
      | e when equal e bot -> Gen.return bottom
      | e ->
          Gen.(
            let x = Elt.Set.choose e in
            let _, _, s = Elt.Set.split x e in
            return s)
    in
    make gen ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module LTests = LCheck.GenericTests (L)
module LTestsTop = LCheck.GenericTopTests (L)

let () =
  Alcotest.run "reverse powerset lattice"
    [
      ("properties", List.map QCheck_alcotest.to_alcotest LTests.suite);
      ("top properties", List.map QCheck_alcotest.to_alcotest LTestsTop.suite);
    ]
