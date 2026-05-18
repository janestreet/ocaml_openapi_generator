open! Core
open! Async

module Code_gen = struct
  type t =
    { raise_on_optional_null : bool [@bool_no_arg]
    (** Use the legacy behavior of raising if an optional field contains null *)
    ; include_unknown_fallback_for_enums : bool [@bool_no_arg]
    (** Include an unknown fallback for enums instead of raising if an unexpected value is
        found *)
    ; maximum_filename_length : Openapi_codegen_ir.Name.Maximum_filename_length.t option
    (** Longest generated filename. Only applies to names generated from the provided
        specs, so it does not for example include the library entry point. Cannot be
        smaller than 32. Defaults to 255. *)
    ; normalize_operation_ids : bool [@bool_no_arg]
    (** Operation names are by default not normalized, so they're not snake cased, and if
        the names are not valid ocaml names, the code might not build. You can opt into
        normalization using this flag. *)
    ; add_sexp_option_annotations : bool [@bool_no_arg]
    (** Annotate fields that are optional in records using [@sexp.option] in additional to
        [@jsonaf.option]. *)
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
