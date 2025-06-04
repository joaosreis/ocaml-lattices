open! Containers
open QCheck

module L : LCheck.LATTICE_TOPLESS = struct
  module L = Int_test
  open Lattices
  module L_1 = Pair.Make (L) (L)
  include L_1
  include Lcheck_helper.Make (L_1)

  let bot = bottom
  let equal x y = leq x y && leq y x
  let name = "pair lattice"

  let arb_elem =
    let gen = Gen.(pair L.gen L.gen) in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      match e with
      | e when equal e bot -> Gen.return bottom
      | e_1, e_2 ->
          Gen.(
            pair L.gen L.gen >>= fun (x, y) ->
            if L.leq x e_1 && L.leq e_1 x then
              if L.leq y e_2 then return (x, y) else gen ()
            else if L.leq x e_1 then return (x, y)
            else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module LTests = LCheck.GenericTests (L)

let () =
  Alcotest.run "pair lattice"
    [ ("properties", List.map QCheck_alcotest.to_alcotest LTests.suite) ]
