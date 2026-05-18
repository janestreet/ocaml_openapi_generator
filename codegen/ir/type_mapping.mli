open! Core

module Matcher : sig
  type t =
    | Type of Openapi_spec.Types.Schema.Type.t
    | Format of Openapi_spec.Types.Schema.Format.t
  [@@deriving sexp, sexp_grammar]
end

type t =
  { matches : Matcher.t Blang.t
  ; description : Type_description.t
  }
[@@deriving sexp, sexp_grammar]

val presets : t list
