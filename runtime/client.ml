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

type 'response t =
  { base_url : string
  ; headers : (string * string) list
  ; secure : bool
  ; make_request :
      ?body:string -> 'response t -> Uri.t -> Method.t -> 'response Deferred.Or_error.t
  ; response_to_string : 'response -> string
  }
[@@deriving fields ~getters]
