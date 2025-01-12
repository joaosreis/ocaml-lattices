open! Core

module Make
    (D : sig
      type t

      include Sexpable.S with type t := t
      module Map : Map.S with type Key.t := t
      module Set : Set.S with type Elt.t := t

      val to_string : t -> string
    end)
    (B : sig
      val bottom_elems : D.Set.t
    end)
    (L : Sig.S) : sig
  include Sig.S with type t = L.t D.Map.t

  val set : t -> D.t -> L.t -> t
  val get : t -> D.t -> L.t option
end
