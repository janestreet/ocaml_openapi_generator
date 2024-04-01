(** A particular HTTP endpoint and method that can be hit. *)

open! Core
open! Async
open Openapi_spec.Types
open Openapi_runtime

type t =
  { operation_id : string
  ; tags : string list
  ; http_method : Httpaf_sexpable.Method.t
  ; path : Path_template.t
  ; summary : string option
  ; description : string option
  ; parameters : Operation_parameter.t list
  ; responses : Operation_response.t list
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

val of_operation
  :  path:string
  -> http_method:Httpaf_sexpable.Method.t
  -> path_parameters:Parameter.t Or_reference.t list
  -> components:Components.t
  -> type_space:Typify.Type_space.t
  -> Operation.t
  -> (t * Typify.Type_space.t) option
