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

module Params : sig
  type t =
    { path : string
    ; query : (string * string list) list
    ; method_ : Method.t
    ; body : string option
    }
end

type ('response, 'metadata) t =
  { metadata : 'metadata
  ; make_request : 'metadata -> Params.t -> 'response Deferred.Or_error.t
  ; response_to_jsonaf : 'response -> Jsonaf.t Or_error.t
  }
[@@deriving fields ~getters]
