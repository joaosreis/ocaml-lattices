open! Containers

module type S = sig
  type t

  val bottom : t
  val leq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val to_string : t -> string
end

module type ELEMENT = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end
