open! Core
open QCheck

module L = struct
  type elem = Bottom | I of int [@@deriving sexp_of]
  type t = elem [@@deriving sexp_of]

  let bot = Bottom
  let bottom = bot

  let leq x y =
    match (x, y) with
    | Bottom, _ -> true
    | _, Bottom -> false
    | I x, I y -> x <= y

  let join x y =
    match (x, y) with Bottom, x | x, Bottom -> x | I x, I y -> I (max x y)

  let meet x y =
    match (x, y) with
    | Bottom, _ | _, Bottom -> Bottom
    | I x, I y -> I (min x y)

  let to_string = function Bottom -> "bottom" | I x -> Int.to_string x

  let eq x y =
    match (x, y) with
    | Bottom, Bottom -> true
    | I x, I y -> x = y
    | I _, Bottom | Bottom, I _ -> false

  let equal = eq

  let arb_elem =
    let gen =
      Gen.(frequency [ (1, return bot); (2, map (fun i -> I i) int) ])
    in
    make gen ~print:to_string

  let arb_elem_le e =
    let rec gen () =
      Gen.(
        match e with
        | Bottom -> return Bottom
        | I _ -> arb_elem.gen >>= fun x -> if leq x e then return x else gen ())
    in
    make (gen ()) ~print:to_string

  let equiv_pair =
    let a = map (fun a -> (a, a)) arb_elem in
    set_print (fun (a, a') -> "(" ^ to_string a ^ ";" ^ to_string a' ^ ")") a

  let name = "int lattice"
end

include L
module LTests = LCheck.GenericTests (L)

let () =
  Alcotest.run "int lattice"
    [ ("properties", List.map ~f:QCheck_alcotest.to_alcotest LTests.suite) ]

let gen = arb_elem.gen
