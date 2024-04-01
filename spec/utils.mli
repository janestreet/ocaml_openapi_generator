open! Core
open! Async
open Types

val resolve_reference
  :  components:Components.t
  -> resolver:(Components.t -> 'a Or_reference.t Core.String.Map.t)
  -> 'a Or_reference.t
  -> 'a option

val resolve_reference_exn
  :  components:Components.t
  -> resolver:(Components.t -> 'a Or_reference.t Core.String.Map.t)
  -> 'a Or_reference.t
  -> 'a

val resolve_schema_ref
  :  components:Components.t
  -> Schema.t Or_reference.t
  -> Schema.t option

val resolve_response_ref
  :  components:Components.t
  -> Response.t Or_reference.t
  -> Response.t option

val resolve_request_body_ref
  :  components:Components.t
  -> Request_body.t Or_reference.t
  -> Request_body.t option

val resolve_parameter_ref
  :  components:Components.t
  -> Parameter.t Or_reference.t
  -> Parameter.t option

val resolve_all_parameters
  :  components:Components.t
  -> Parameter.t Or_reference.t list
  -> (string, Parameter.t) Base.Hashtbl.t
