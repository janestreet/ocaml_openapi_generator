open! Core
open Async

module Method : sig
  type t =
    [ `GET
    | `HEAD
    | `POST
    | `PUT
    | `DELETE
    | `CONNECT
    | `OPTIONS
    | `TRACE
    | `Other of string
    ]

  include Stringable.S with type t := t
end

type 'response t =
  { base_url : string
  ; headers : (string * string) list
  ; secure : bool
  ; make_request :
      ?body:string -> 'response t -> Uri.t -> Method.t -> 'response Deferred.Or_error.t
  ; response_to_string : 'response -> string
  }
[@@deriving fields ~getters]
