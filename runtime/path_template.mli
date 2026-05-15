open! Core

module Path_component : sig
  type t =
    | Constant of string
    | Parameter of string
end

type t = Path_component.t list [@@deriving sexp]

val of_template_string : string -> t option
val encode_path : string list -> string
val render_path : ?parameters:string String.Map.t -> t -> string option
