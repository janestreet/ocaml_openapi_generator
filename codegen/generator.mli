open! Core
open! Async
open Openapi_spec.Types
open Openapi_codegen_ir
open Openapi_codegen_ir.Typify

val type_reference : type_id:Type_id.t -> type_space:Type_space.t -> string
val type_definition : type_id:Type_id.t -> type_space:Type_space.t -> string option

val make_operation_definition_ml
  :  operation_list:Operation_method.t list
  -> type_space:Type_space.t
  -> string

val make_operation_method_list
  :  type_space:Type_space.t
  -> paths:Paths.t
  -> components:Components.t
  -> Operation_method.t list * Type_space.t

val make_type_mls : type_space:Type_space.t -> (string * Name.t) list
val make_jbuild : name:string -> string
val make_files : config:Config.t -> api:Open_api.t -> unit Deferred.Or_error.t
