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
end) : S with module Elt = D and type t = Set.M(D).t

module Make_reverse (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) : sig
  include S with module Elt = D and type t = Set.M(D).t

  val top : t
end
