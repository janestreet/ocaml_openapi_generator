open! Core

module Namespace : sig
  type t =
    private
    [ `Functor_application of string * t | `Module of string ] Nonempty_list.t
  [@@deriving sexp, string, compare, sexp_grammar]

  include Comparable.S_plain with type t := t
end

module Identifier : sig
  type t = private
    { namespace : Namespace.t option
    ; name : string
    }
  [@@deriving sexp, string, compare, sexp_grammar]

  include Comparable.S_plain with type t := t
end

type t =
  { name : Identifier.t
  ; arguments : t list
  ; needs_string_primitives : bool
  }
[@@deriving sexp, sexp_grammar, equal]

val create : ?arguments:t list -> ?needs_string_primitives:bool -> Identifier.t -> t
val generated : Name.t -> maximum_filename_length:Name.Maximum_filename_length.t -> t
val option : t -> t
val list : t -> t
val string_assoc : t -> t

(** Serializers *)

val type_name : t -> string

module Presets : sig
  val string : t
  val int : t
  val int64 : t
  val float : t
  val bool : t
  val jsonaf : t
  val ip : t
  val ipv4 : t
  val ipv6 : t
  val time : t
  val uuid : t
end
