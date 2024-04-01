open! Core

type t =
  | Octet_stream
  | Json
  | Form_urlencoded
[@@deriving sexp]

val of_string : string -> t Or_error.t
