open! Core
open! Async
open Types

let rec resolve_reference
  : type a.
    components:Components.t
    -> resolver:(Components.t -> a Or_reference.t String.Map.t)
    -> a Or_reference.t
    -> a option
  =
  fun ~components ~resolver ref_ ->
  match ref_ with
  | Ref ref_ ->
    let reference = Reference.ref_ ref_ in
    let%bind.Option last_segment = Reference.last_segment reference in
    let component_map = resolver components in
    Map.find component_map last_segment
    |> Option.bind ~f:(resolve_reference ~components ~resolver)
  | Value v -> Some v
;;

let resolve_reference_exn ~components ~resolver ref_ =
  resolve_reference ~components ~resolver ref_ |> Option.value_exn
;;

(* Convenience wrapper functions around resolve_reference... *)

let resolve_schema_ref ~components =
  resolve_reference ~components ~resolver:Components.schemas
;;

let resolve_response_ref ~components =
  resolve_reference ~components ~resolver:Components.responses
;;

let resolve_request_body_ref ~components =
  resolve_reference ~components ~resolver:Components.request_bodies
;;

let resolve_parameter_ref ~components =
  resolve_reference ~components ~resolver:Components.parameters
;;

(* Convenience functions for lists... *)

let resolve_all_parameters ~components parameters =
  List.map parameters ~f:(fun parameter_or_ref ->
    let parameter =
      resolve_parameter_ref ~components parameter_or_ref |> Option.value_exn
    in
    Parameter.Parameter_data.name (Parameter.data parameter), parameter)
  |> Hashtbl.of_alist_exn (module String)
;;
