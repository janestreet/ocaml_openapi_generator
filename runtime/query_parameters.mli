open! Core

type t

val to_list : t -> (string * string list) list
val concat : t list -> t
val singleton : ('a -> string) -> key:string -> value:'a -> t

val array
  :  ?explode:bool
  -> ?style:Openapi_spec.Types.Parameter.Query_style.t
  -> ('a -> string)
  -> key:string
  -> value:'a list
  -> t

val object_
  :  ?explode:bool
  -> ?style:Openapi_spec.Types.Parameter.Query_style.t
  -> unit
  -> key:string
  -> value:(string * Jsonaf.t) list
  -> t
