open! Core
open! Async

module Code_gen = struct
  type t =
    { raise_on_optional_null : bool [@bool_no_arg]
    (** Use the legacy behavior of raising if an optional field contains null *)
    ; include_unknown_fallback_for_enums : bool [@bool_no_arg]
    (** Include an unknown fallback for enums instead of raising if an unexpected value is
        found *)
    }
  [@@deriving sexp, fields ~getters ~iterators:make_creator, roundtrippable_command_param]
end

type t =
  { name : string (** project name *)
  ; destination : Filename.t [@name "out"] (** the output directory *)
  ; spec_file : Filename.t [@name "spec"] (** a v3.0.3 spec file *)
  ; generated_files_archive : Filename.t option
  (** By default, the generator will output all the ml files as they are in the directory.
      However, that complicated build rules when the schema or code generator are updated,
      so if you pass in this argument, the ml files will be archived to the given filename
      instead. *)
  ; code_gen : Code_gen.t [@roundtrippable_command_param]
  }
[@@deriving
  sexp
  , fields ~getters ~setters ~iterators:(create, make_creator)
  , roundtrippable_command_param]

let create = Fields.create
