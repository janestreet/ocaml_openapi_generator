open! Core
open! Async
open Openapi_spec.Types

(** References a type in a [Type_space.t] *)
module Type_id : sig
  type t [@@deriving sexp]

  module Map : Map_intf.S with type Key.t = t
end

(** Describes how a variant should be serialized *)
module Variant_structures : sig
  type t =
    | String_enum (** a single string, from a set of possible strings *)
    | Tagged_object of string (** an object tagged at a given key *)
    | Transparent_object (** an object with no identifier *)
  [@@deriving sexp]
end

(** How a type is constructed in OCaml *)
module Type_structure : sig
  type t =
    | String_variant of Type_id.t list
    | Object_variant of (Type_id.t * Name.t) list * string
    | Transparent_variant of (Type_id.t * Name.t) list
    | Record of Type_id.t Name.Map.t
    | Specific_string of string
    | List of Type_id.t
    | Set of Type_id.t
    | Optional of Type_id.t
    | Nullable of Type_id.t
    | Existing_type
end

module Type : sig
  type t =
    { name : Name.t (** module name that should be generated *)
    ; structure : Type_structure.t
    ; variant_name : Name.t option (** name of variant branch that this type is from *)
    }
  [@@deriving fields ~getters ~setters, sexp]

  val create
    :  ?namespace:string
    -> ?variant_name:string
    -> name:string
    -> structure:Type_structure.t
    -> unit
    -> t
end

module Type_space : sig
  type t [@@deriving sexp]

  val empty : t
  val type_of_id : type_id:Type_id.t -> t -> Type.t option

  val add_schema
    :  ?name:
         string
         (* [singleton_enum_in_variant] is meant to indicate that the schema being
            processed is an enum with just one element in it, and it belongs to a list of
            such singleton enums in a oneOf. [add_schema] will treat this list as a
            variant type. See the expect_test in ../test/tests.ml "list of enums in a
            oneOf translates to a variant type" for an example *)
    -> ?singleton_enum_in_variant:bool
    -> schema:Schema.t Or_reference.t
    -> components:Components.t
    -> t
    -> Type_id.t * t

  val to_map : t -> Type.t Type_id.Map.t
end
