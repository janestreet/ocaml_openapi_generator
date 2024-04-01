open! Core

module Status : sig
  include module type of Httpaf.Status
  include Sexpable.S with type t := Httpaf.Status.t
end

module Method : sig
  include module type of Httpaf.Method
  include Sexpable.S with type t := Httpaf.Method.t
end
