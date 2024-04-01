open! Core
open! Async

type t =
  { name : string
  ; destination : Filename.t
  }
[@@deriving sexp, fields ~getters ~setters ~iterators:create]

let create = Fields.create
