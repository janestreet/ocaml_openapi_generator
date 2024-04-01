(** The format of a request or response body (e.x. "JSON") *)

open! Core
open! Async

type t =
  | Octet_stream
  | Json
  | Form_urlencoded
[@@deriving sexp]

let of_string = function
  | "application/x-www-form-urlencoded" -> Ok Form_urlencoded
  | "application/json" -> Ok Json
  | "application/octet-stream" -> Ok Octet_stream
  | other -> Or_error.error_string ("unknown content type: " ^ other)
;;
