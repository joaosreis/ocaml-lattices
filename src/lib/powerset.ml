open! Containers

module type ELT = sig
  type t

  module Set : Set.S with type elt := t

  val to_string : t -> string
end

module type S = sig
  module Elt : ELT

  type t = Elt.Set.t

  include Sig.S with type t := t
end

module Make (D : ELT) = struct
  module Elt = D
  module Set = Elt.Set

  type t = Elt.Set.t

  let bottom = Set.empty
  let join = Set.union
  let meet = Set.inter
  let leq x y = Set.subset x y

  let to_string x =
    let f x = [%string "%{x#Elt};"] in
    List.fold_left (fun acc x -> acc ^ f x) "" (Set.to_list x)
end

module Make_reverse
    (D : ELT)
    (B : sig
      val bottom : D.Set.t
    end) =
struct
  module Elt = D
  module Set = D.Set

  let bottom = Set.to_list B.bottom

  type t = Set.t

  let bottom = Set.of_list bottom
  let top = Set.empty
  let join = Set.inter
  let meet = Set.union
  let leq x y = Set.equal x y || not (Set.subset x y)

  let to_string x =
    let f x = [%string "%{x#D};"] in
    List.fold_left (fun acc x -> acc ^ f x) "" (Set.to_list x)
end
