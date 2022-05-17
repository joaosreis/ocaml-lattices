open! Core

module Make (D : sig
  include Comparable.S

  include Sexpable.S with type t := t

  val to_string : t -> string
end) (B : sig
  val bottom_elems : Set.M(D).t
end)
(L : Sig.S) : sig
  include Sig.S with type t = L.t Map.M(D).t

  val set : t -> D.t -> L.t -> t

  val get : t -> D.t -> L.t option
end
