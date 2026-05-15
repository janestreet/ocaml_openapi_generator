open! Core

type t [@@deriving sexp]

include Comparable.S with type t := t

val of_raw_string : string -> t
val of_operation_path : string -> t
val to_variant : t -> string
val to_variable_name : t -> string
val to_raw_string : t -> string

module Maximum_filename_length : sig
  type t = private int [@@deriving sexp, equal]

  val smallest_possible_value : int
  val default : t
  val of_int_exn : int -> t
  val roundtrippable_arg_type : t Roundtrippable_arg_type.t
end

val to_module_name
  :  t
  -> kind:[ `Full | `Truncated of Maximum_filename_length.t ]
  -> string

val to_filename : t -> maximum_filename_length:Maximum_filename_length.t -> string
val filenames_equal : t -> t -> maximum_filename_length:Maximum_filename_length.t -> bool
