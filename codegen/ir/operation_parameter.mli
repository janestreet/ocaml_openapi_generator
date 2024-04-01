(** A request body, path parameter, query parameter, or header. *)

open! Core
open! Async
open Typify
open Openapi_spec.Types

module Operation_parameter_type : sig
  type t =
    | Type of Type_id.t
    | Raw_body
  [@@deriving sexp]
end

module Operation_parameter_kind : sig
  type t =
    | Path
    | Query of bool
    | Header of bool
    | Body of Body_content_type.t
  [@@deriving sexp]
end

type t =
  { name : Name.t
  ; description : string option
  ; type_ : Operation_parameter_type.t
  ; kind : Operation_parameter_kind.t
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

val create
  :  name:string
  -> description:string option
  -> type_:Operation_parameter_type.t
  -> kind:Operation_parameter_kind.t
  -> t

val of_body
  :  ?name:string
  -> components:Components.t
  -> type_space:Type_space.t
  -> Request_body.t
  -> (t * Type_space.t) option
