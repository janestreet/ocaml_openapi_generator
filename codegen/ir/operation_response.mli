(** A response type for an operation. *)

open! Core
open! Async
open Typify

module Operation_response_status : sig
  type t =
    | Code of Httpaf_sexpable.Status.t
    | Range of int
    | Default
  [@@deriving sexp]

  val of_string : string -> t
end

module Operation_response_type : sig
  type t =
    | Resolved of Type_id.t
    | None
    | Raw
    | Upgrade
  [@@deriving sexp]
end

type t =
  { status_code : Operation_response_status.t
  ; type_id : Operation_response_type.t
  ; description : string option
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

val create
  :  status_code:Operation_response_status.t
  -> type_id:Operation_response_type.t
  -> description:string option
  -> t

val get_default : t list -> t option
