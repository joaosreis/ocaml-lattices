open! Core

module Make (L : Sig.S) : Sig.S with type t = L.t list
