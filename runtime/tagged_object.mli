open! Core
open! Async

(** insert a string value at a given tag inside a JSON object *)
val inject : tag:string -> value:string -> Jsonaf.t -> Jsonaf.t

(** retrieve the string value at a given tag inside a JSON object *)
val retrieve : tag:string -> Jsonaf.t -> string * Jsonaf.t
