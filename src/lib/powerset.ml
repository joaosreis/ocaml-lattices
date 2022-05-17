open! Core

module type S = sig
  module Elt : Comparable.S

  type t = Set.M(Elt).t

  include Sig.S with type t := t
end

module Make (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) =
struct
  module Elt = D

  type t = Set.M(Elt).t [@@deriving sexp_of]

  let bottom = Set.empty (module Elt)

  let join = Set.union

  let meet = Set.inter

  let leq x y = Set.is_subset x ~of_:y

  let to_string x =
    let f x = [%string "%{x#Elt};"] in
    List.fold_left (Set.to_list x) ~f:(fun acc x -> acc ^ f x) ~init:""
end

module Make_reverse (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) =
struct
  module Elt = D

  let bottom = Set.to_list B.bottom

  type t = Set.M(Elt).t [@@deriving sexp_of]

  let bottom = Set.of_list (module Elt) bottom

  let top = Set.empty (module Elt)

  let join = Set.inter

  let meet = Set.union

  let leq x y = Set.equal x y || not (Set.is_subset x ~of_:y)

  let to_string x =
    let f x = [%string "%{x#D};"] in
    List.fold_left (Set.to_list x) ~f:(fun acc x -> acc ^ f x) ~init:""
end
