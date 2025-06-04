open! Containers

module Make
    (L : Sig.S)
    (N : sig
      val n : int
    end) : Sig.S with type t := L.t list
