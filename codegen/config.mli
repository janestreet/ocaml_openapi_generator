open! Core
open! Async

module Code_gen : sig
  type t =
    { raise_on_optional_null : bool
    ; include_unknown_fallback_for_enums : bool
    }
  [@@deriving sexp, fields ~getters ~iterators:make_creator, roundtrippable_command_param]
end

type t =
  { name : string
  ; destination : Filename.t
  ; spec_file : Filename.t
  ; generated_files_archive : Filename.t option
  ; code_gen : Code_gen.t
  }
[@@deriving
  sexp
  , fields ~getters ~setters ~iterators:(create, make_creator)
  , roundtrippable_command_param]

val name : t -> string

val create
  :  name:string
  -> destination:Filename.t
  -> spec_file:Filename.t
  -> generated_files_archive:Filename.t option
  -> code_gen:Code_gen.t
  -> t
