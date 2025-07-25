open! Containers
open QCheck

module Int = struct
  include Int
  module Set = Set.Make (Int)
end

module L : LCheck.LATTICE_TOPLESS = struct
  module L_1 = Lattices.Powerset.Make (Int)
  include L_1
  include Lcheck_helper.Make (L_1)

  let bot = bottom
  let equal x y = leq x y && leq y x
  let name = "powerset lattice"

  let arb_elem =
    let gen = Gen.(map Elt.Set.of_list (list int)) in
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

let () =
  Alcotest.run "powerset lattice"
    [ ("properties", List.map QCheck_alcotest.to_alcotest LTests.suite) ]
