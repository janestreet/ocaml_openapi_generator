open! Core

type t =
  | Octet_stream
  | Json
  | Form_urlencoded
  | Multipart_form_data
  | Multipart_mixed
[@@deriving sexp]

val of_string : string -> t Or_error.t
