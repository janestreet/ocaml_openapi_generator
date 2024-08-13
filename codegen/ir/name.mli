open! Core

type t [@@deriving sexp]

include Comparable.S with type t := t

val of_raw_string : ?namespace:string -> string -> t
val to_module_name : t -> string
val to_variant : t -> string
val to_variable_name : t -> string
val to_raw_string : t -> string
val to_truncated_name : t -> string
val of_operation_path : string -> t
val filenames_equal : t -> t -> bool
