open! Core
open! Async

type t =
  { name : string
  ; destination : Filename.t
  }
[@@deriving sexp, fields ~getters ~setters ~iterators:create]

val name : t -> string
val create : name:string -> destination:Filename.t -> t
