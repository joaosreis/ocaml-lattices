open! Containers
open Lattices
open QCheck

module L : LCheck.LATTICE = struct
  include Interval
  include Lcheck_helper.Make (Interval)

  let name = "interval lattice"

  let gen_lbound =
    Gen.(frequency [ (1, return `LInf); (2, map (fun i -> `Int i) int) ])

  let gen_hbound =
    Gen.(frequency [ (1, return `HInf); (2, map (fun i -> `Int i) int) ])

  let rec gen_hbound_le : lower_bound * upper_bound -> upper_bound Gen.t =
   fun i ->
    match i with
    | l, `HInf ->
        Gen.(
          gen_hbound >>= fun x ->
          if bound_leq l x then return x else gen_hbound_le i)
    | l, (`Int _ as e) ->
        Gen.(
          gen_hbound >>= fun x ->
          if bound_leq l x && bound_leq x e then return x else gen_hbound_le i)

  let rec gen_lbound_ge : lower_bound * upper_bound -> lower_bound Gen.t =
   fun i ->
    match i with
    | `LInf, h ->
        Gen.(
          gen_lbound >>= fun x ->
          if bound_leq x h then return x else gen_lbound_ge i)
    | (`Int _ as e), h ->
        Gen.(
          gen_lbound >>= fun x ->
          if bound_leq x h && bound_leq e x then return x else gen_lbound_ge i)

  let arb_elem =
    let mk_element : lower_bound -> upper_bound -> t =
     fun x y ->
      match (x, y) with
      | `Int x, `Int y ->
          if x <= y then Interval (`Int x, `Int y) else Interval (`Int y, `Int x)
      | _ -> Interval (x, y)
    in
    let gen =
      Gen.(
        frequency
          [
            (1, return bottom);
            (1, return top);
            (2, map2 mk_element gen_lbound gen_hbound);
          ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let mk_element : lower_bound * upper_bound -> t = function
      | `Int x, `Int y ->
          if x <= y then Interval (`Int x, `Int y) else Interval (`Int y, `Int x)
      | x, y -> Interval (x, y)
    in
    let gen =
      match e with
      | Empty -> Gen.return bottom
      | e when eq top e -> arb_elem.gen
      | Interval (l, h) ->
          Gen.(
            map mk_element (pair (gen_lbound_ge (l, h)) (gen_hbound_le (l, h))))
    in
    make gen ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a
end

module LTests = LCheck.GenericTests (L)
module LTestsTop = LCheck.GenericTopTests (L)

let () =
  Alcotest.run "interval lattice"
    [
      ("properties", List.map QCheck_alcotest.to_alcotest LTests.suite);
      ("top properties", List.map QCheck_alcotest.to_alcotest LTestsTop.suite);
    ]
