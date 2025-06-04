open! Containers

type lower_bound = [ `LInf | `Int of int ]
type upper_bound = [ `HInf | `Int of int ]
type t = Empty | Interval of lower_bound * upper_bound

val bound_leq :
  [< `LInf | `HInf | `Int of int ] -> [< `LInf | `HInf | `Int of int ] -> bool

val bound_to_string : [< `LInf | `HInf | `Int of int ] -> string

include Sig.S with type t := t

val top : t
val meet : t -> t -> t
