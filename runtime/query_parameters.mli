open! Core

type t

val concat_to_list : t list -> (string * string list) list
val singleton : ('a -> string) -> key:string -> value:'a -> t

val array
  :  ?explode:bool
  -> ?style:Openapi_spec.Types.Parameter.Query_style.t
  -> ('a -> string)
  -> key:string
  -> value:'a list
  -> t

val map
  :  ?explode:bool
  -> ?style:Openapi_spec.Types.Parameter.Query_style.t
  -> ('a -> string)
  -> key:string
  -> value:(string * 'a) list
  -> t

val object_
  :  ?explode:bool
  -> ?style:Openapi_spec.Types.Parameter.Query_style.t
  -> unit
  -> key:string
  -> value:(string * Jsonaf.t) list
  -> t

(** Optionality combinators. Optional drops the value. Nullable defaults to null. *)

val optional : (key:string -> value:'a -> t) -> key:string -> value:'a option -> t
val make_nullable : ('a -> string) -> 'a option -> string
