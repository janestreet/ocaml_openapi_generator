(** The format of a request or response body (e.x. "JSON") *)

open! Core
open! Async

type t =
  | Octet_stream
  | Json
  | Form_urlencoded
  | Multipart_form_data
  | Multipart_mixed
[@@deriving sexp]

let of_string = function
  | "application/x-www-form-urlencoded" -> Ok Form_urlencoded
  | "application/json" -> Ok Json
  | "application/octet-stream" -> Ok Octet_stream
  | "multipart/form-data" -> Ok Multipart_form_data
  | "multipart/mixed" -> Ok Multipart_mixed
  | other -> Or_error.error_string ("unknown content type: " ^ other)
;;
