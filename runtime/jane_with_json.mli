open! Core
open! Async
open! Jsonaf.Export

module Jsonaf_int64 : sig
  include module type of Int64

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_int : sig
  include module type of Int

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_float : sig
  include module type of Float.Stable.V1

  val to_string : t -> string
  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_bool : sig
  include module type of Bool.Stable.V1

  val to_string : t -> string
  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_ip : sig
  include module type of Unix.Inet_addr.Stable.V1
  include Stringable.S with type t := t

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_ipv4 : sig
  include module type of Unix.Inet_addr.Stable.V1
  include Stringable.S with type t := t

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_ipv6 : sig
  include module type of Unix.Inet_addr.Stable.V1
  include Stringable.S with type t := t

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_time : sig
  include module type of Time_ns_unix.Stable.V1
  include Stringable.S with type t := t

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_uuid : sig
  include module type of Uuid.Stable.V1
  include Stringable.S with type t := t

  val t_of_jsonaf : Jsonaf.t -> t
  val jsonaf_of_t : t -> Jsonaf.t
end

module Jsonaf_string : sig
  include module type of String.Stable.V1

  val t_of_jsonaf : Jsonaf.t -> t
  val jsonaf_of_t : t -> Jsonaf.t
end

module Assoc : sig
  type ('key, 'value) t = ('key * 'value) list [@@deriving sexp, compare]

  module M (Key : sig
      type t
    end) : sig
    type nonrec 'value t = (Key.t, 'value) t
  end

  module type Sexp_of_m = sig
    type t [@@deriving sexp_of]
  end

  val sexp_of_m__t
    :  (module Sexp_of_m with type t = 'key)
    -> ('value -> Sexp.t)
    -> ('key, 'value) t
    -> Sexp.t

  module type M_of_sexp = sig
    type t [@@deriving of_sexp]
  end

  val m__t_of_sexp
    :  (module M_of_sexp with type t = 'key)
    -> (Sexp.t -> 'value)
    -> Sexp.t
    -> ('key, 'value) t

  module type Jsonaf_of_m = sig
    type t [@@deriving to_string]
  end

  val jsonaf_of_m__t
    :  (module Jsonaf_of_m with type t = 'key)
    -> ('value -> Jsonaf.t)
    -> ('key, 'value) t
    -> Jsonaf.t

  module type M_of_jsonaf = sig
    type t [@@deriving of_string]
  end

  val m__t_of_jsonaf
    :  (module M_of_jsonaf with type t = 'key)
    -> (Jsonaf.t -> 'value)
    -> Jsonaf.t
    -> ('key, 'value) t

  module type Compare_m = sig
    type t [@@deriving compare]
  end

  val compare_m__t
    :  (module Compare_m with type t = 'key)
    -> ('value -> 'value -> int)
    -> ('key, 'value) t
    -> ('key, 'value) t
    -> int
end

module With_additional_properties (Serializable : sig
    type result
    type t [@@deriving jsonaf]

    val jsonaf_fields_of_t : string list

    module Additional_properties_content : sig
      type t [@@deriving jsonaf]
    end

    val of_serializable : t -> (string * Additional_properties_content.t) list -> result
    val to_serializable : result -> t * (string * Additional_properties_content.t) list
  end) : Jsonaf.Jsonafable.S with type t := Serializable.result
