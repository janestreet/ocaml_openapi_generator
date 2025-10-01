open! Core
open Async

module Method = struct
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

  let of_string s =
    match String.uppercase s with
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | "POST" -> `POST
    | "PUT" -> `PUT
    | "DELETE" -> `DELETE
    | "CONNECT" -> `CONNECT
    | "OPTIONS" -> `OPTIONS
    | "TRACE" -> `TRACE
    | http_method -> `Other http_method
  ;;

  let to_string t =
    match t with
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | `DELETE -> "DELETE"
    | `CONNECT -> "CONNECT"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `Other s -> s
  ;;
end

module Params = struct
  type t =
    { path : string
    ; query : (string * string list) list
    ; method_ : Method.t
    ; body : string option
    }
  [@@deriving sexp, equal]
end

module Content_type_and_bytes = struct
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
