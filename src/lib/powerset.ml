open! Core

module type ELT = sig
  type t

  module Set : Set.S with type Elt.t := t

  val to_string : t -> string
end

module type S = sig
  module Elt : ELT

  type t = Elt.Set.t

  include Sig.S with type t := t
end

module Make (D : ELT) = struct
  module Elt = D

  type t = Elt.Set.t [@@deriving sexp_of]

  let bottom = Elt.Set.empty

  let join = Elt.Set.union

  let meet = Elt.Set.inter

  let leq x y = Elt.Set.is_subset x ~of_:y

  let to_string x =
    let f x = [%string "%{x#Elt};"] in
    List.fold_left (Set.to_list x) ~f:(fun acc x -> acc ^ f x) ~init:""
end

module Make_reverse
    (D : ELT) (B : sig
      val bottom : D.Set.t
    end) =
struct
  module Elt = D

  let bottom = Set.to_list B.bottom

  type t = Elt.Set.t [@@deriving sexp_of]

  let bottom = Elt.Set.of_list bottom

  let top = Elt.Set.empty

  let join = Elt.Set.inter

  let meet = Elt.Set.union

  let leq x y = Elt.Set.equal x y || not (Elt.Set.is_subset x ~of_:y)

  let to_string x =
    let f x = [%string "%{x#D};"] in
    List.fold_left (Elt.Set.to_list x) ~f:(fun acc x -> acc ^ f x) ~init:""
end
