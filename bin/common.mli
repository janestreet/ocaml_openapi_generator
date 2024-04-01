open! Core
open! Async
open Openapi_spec.Types

val parse_openapi : string -> Open_api.t Base.Or_error.t Deferred.t
val read_specfile : file:Filename.t -> Open_api.t Base.Or_error.t Deferred.t
