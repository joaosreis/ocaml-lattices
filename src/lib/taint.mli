open! Core

include Sig.S with type t = bool Flat.flat

val join_taint : t -> t -> t
