open! Core

module type S = sig
  type elt
  type t = [ `Top | `Some of elt ]

  include Sig.S with type t := t

  val top : t
end

module Make (L : Sig.S) : S with type elt := L.t
