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

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_ipv4 : sig
  include module type of Unix.Inet_addr.Stable.V1

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_ipv6 : sig
  include module type of Unix.Inet_addr.Stable.V1

  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_time : sig
  include module type of Time_ns_unix.Stable.V1

  val to_string : t -> string
  val jsonaf_of_t : t -> Jsonaf.t
  val t_of_jsonaf : Jsonaf.t -> t
end

module Jsonaf_uuid : sig
  include module type of Uuid.Stable.V1

  val t_of_jsonaf : Jsonaf.t -> t
  val jsonaf_of_t : t -> Jsonaf.t
end

module Jsonaf_string : sig
  include module type of String.Stable.V1

  val t_of_jsonaf : Jsonaf.t -> t
  val jsonaf_of_t : t -> Jsonaf.t
end
