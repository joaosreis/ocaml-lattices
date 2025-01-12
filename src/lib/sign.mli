open! Core

type sign = Zero | Pos | Neg [@@deriving sexp_of]
type t = sign Flat.flat

include Sig.S with type t := t

val top : t
val plus : t -> t -> t
val minus : t -> t -> t
val times : t -> t -> t
val divide : t -> t -> t
val sign : int -> t
val sign_bigint : Bigint.t -> t
