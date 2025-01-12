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

module Make (D : ELT) : S with module Elt = D

module Make_reverse
    (D : ELT)
    (B : sig
      val bottom : D.Set.t
    end) : sig
  include S with module Elt = D

  val top : t
end
