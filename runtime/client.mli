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
  [@@deriving sexp, equal]

  include Stringable.S with type t := t
end

module Params : sig
  type t =
    { path : string
    ; query : (string * string list) list
    ; method_ : Method.t
    ; body : string option
    }
  [@@deriving sexp, equal]
end

module Content_type_and_bytes : sig
  type t =
    { content_type : string option
    ; bytes : string
    }
end

type ('response, 'metadata) t =
  { metadata : 'metadata
  ; make_request : 'metadata -> Params.t -> 'response Deferred.Or_error.t
  ; response_to_jsonaf : 'response -> Jsonaf.t Or_error.t
  ; response_to_bytes : 'response -> Content_type_and_bytes.t Or_error.t
  }
[@@deriving fields ~getters]
