open! Core
open! Async
open Openapi_spec.Types
open Openapi_codegen_ir
open Openapi_codegen_ir.Typify

val type_definition
  :  type_id:Type_id.t
  -> type_space:Type_space.t
  -> code_gen_config:Config.Code_gen.t
  -> string option

val make_operation_definition_ml
  :  operation_list:Operation_method.t list
  -> type_space:Type_space.t
  -> maximum_filename_length:Name.Maximum_filename_length.t
  -> normalize_operation_ids:bool
  -> string

val make_operation_definition_mli
  :  operation_list:Operation_method.t list
  -> type_space:Type_space.t
  -> maximum_filename_length:Name.Maximum_filename_length.t
  -> normalize_operation_ids:bool
  -> propagate_descriptions:bool
  -> string

val make_operation_method_list
  :  type_space:Type_space.t
  -> paths:Paths.t
  -> components:Components.t
  -> Operation_method.t list * Type_space.t

(** Renders the [operations.ml] / [operations.mli] index: builds the nested operation
    module tree, checks for node-level function-name collisions (raising on failure), and
    renders it. Exposed for tests; [make_files] performs the equivalent steps. *)
val make_operations_index
  :  type_space:Type_space.t
  -> paths:Paths.t
  -> components:Components.t
  -> maximum_filename_length:Name.Maximum_filename_length.t
  -> normalize_operation_ids:bool
  -> for_mli:bool
  -> string

val make_type_mls
  :  type_space:Type_space.t
  -> code_gen_config:Config.Code_gen.t
  -> (string * Name.t) list

val make_type_mlis
  :  type_space:Type_space.t
  -> code_gen_config:Config.Code_gen.t
  -> (string * Name.t) list

val make_jbuild : config:Config.t -> paths:Set.M(String).t -> string
val make_files : config:Config.t -> api:Open_api.t -> unit Deferred.Or_error.t
