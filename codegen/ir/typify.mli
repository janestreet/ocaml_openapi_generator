open! Core
open! Async
open Openapi_spec.Types

(** References a type in a [Type_space.t] *)
module Type_id : sig
  type t [@@deriving sexp, hash, compare]

  module Map : Map_intf.S with type Key.t = t
end

(** How a type is constructed in OCaml *)
module Type_structure : sig
  type t =
    | String_variant of Name.t list
    | Object_variant of (Type_id.t * Name.t) list * string
    | Transparent_variant of (Type_id.t * Name.t) list
    | Record of
        { properties : (Type_id.t * [ `Required | `Optional ]) Name.Map.t
        ; additional_properties : [ `Not_allowed | `Explicit of Type_id.t | `Allowed ]
        }
    | List of Type_id.t
    | Map of Type_id.t
    | Nullable of Type_id.t
    | Existing_type of Type_description.t
end

module Parameter_origin : sig
  type t =
    | Component of { name : string }
    | Path of
        { path : string
        ; parameter_name : string
        }
    | Operation of
        { operation_id : string
        ; parameter_name : string
        }
  [@@deriving sexp_of, compare]
end

module Response_origin : sig
  type t =
    | Component of { name : string }
    | Operation of
        { operation_id : string
        ; status : Operation_response_status.t
        }
  [@@deriving sexp_of, compare]
end

module Type : sig
  type t =
    { name : Name.t (** module name that should be generated *)
    ; structure : Type_structure.t
    }
  [@@deriving fields ~getters ~setters, sexp]

  val create : name:string -> structure:Type_structure.t -> unit -> t
end

module Type_space : sig
  type t [@@deriving sexp]

  val empty : t
  val type_of_id : type_id:Type_id.t -> t -> Type.t option

  val add_schema
    :  ?name:string
    -> schema:Schema.t Or_reference.t
    -> components:Components.t
    -> t
    -> Type_id.t * t

  val add_schema_for_parameter
    :  t
    -> schema:Schema.t Or_reference.t
    -> origin:Parameter_origin.t
    -> components:Components.t
    -> Type_id.t * t

  val add_schema_for_response
    :  t
    -> schema:Schema.t Or_reference.t
    -> origin:Response_origin.t
    -> components:Components.t
    -> success_response_for_operation:bool
         (** Per [Operation_response_status.pick_primary_success_response] *)
    -> Type_id.t * t

  val to_map : t -> Type.t Type_id.Map.t
end
