open! Core

module Path_component : sig
  type t = Openapi_path_template.Path_component.t =
    | Constant of string
    | Parameter of string
  [@@deriving sexp, equal]
end

type t = Path_component.t list [@@deriving sexp, equal]

val of_template_string : string -> t option
val encode_path : string list -> string
val render_path : ?parameters:string String.Map.t -> t -> string option
