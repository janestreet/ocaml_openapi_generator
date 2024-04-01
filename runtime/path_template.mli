open! Core

type t [@@deriving sexp]

val of_template_string : string -> t option
val render_path : ?parameters:string String.Map.t -> t -> string option
