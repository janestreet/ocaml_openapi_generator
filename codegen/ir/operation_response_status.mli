open! Core

module Range : sig
  type t =
    | Informational
    | Successful
    | Redirection
    | Client_error
    | Server_error
  [@@deriving compare, string, sexp]
end

type t =
  | Default
  | Range of Range.t
  | Code of Httpaf.Status.t
[@@deriving sexp, string, compare]

(** Picks a canonical success response from a list of statuses, preferring the following
    in order:
    1. Successful status code. If many are available, pick the lowest one.
    2. Successful range.
    3. Default.

    If the list is empty, or if none of the response are successful, returns [None]. *)
val pick_primary_success_response : t list -> t option

include Comparable.S_plain with type t := t
