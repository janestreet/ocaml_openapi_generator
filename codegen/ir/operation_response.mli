(** A response type for an operation. *)

open! Core
open! Async
open Typify

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
  ; success_response_for_operation : bool
  (** Per [Operation_response_status.pick_primary_success_response] *)
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

val create
  :  status_code:Operation_response_status.t
  -> type_id:Operation_response_type.t
  -> description:string option
  -> success_response_for_operation:bool
  -> t

val get_success_response_for_operation : t list -> t option
