open! Core
open! Async

module Code_gen = struct
  module Extra_derivers = struct
    type t =
      { record : string list [@name "extra-record-derivers"]
      (** Appended to every generated record type (e.g. [bin_io], [hash]). *)
      ; string_variant : string list [@name "extra-string-variant-derivers"]
      (** Appended to every generated string-enum variant type (e.g. [enumerate]). *)
      ; object_variant : string list [@name "extra-object-variant-derivers"]
      (** Appended to every generated tagged-object variant type. *)
      ; transparent_variant : string list [@name "extra-transparent-variant-derivers"]
      (** Appended to every generated transparent variant type. *)
      }
    [@@deriving
      sexp, fields ~getters ~iterators:make_creator, roundtrippable_command_param]

    let empty =
      { record = []; string_variant = []; object_variant = []; transparent_variant = [] }
    ;;
  end

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
    ; propagate_descriptions : bool [@bool_no_arg]
    (** Propagate [description] fields from the spec into odoc doc comments in the
        generated [.mli] interfaces (types, record fields, variant constructors, and
        operations). *)
    ; extra_derivers : Extra_derivers.t [@roundtrippable_command_param]
    (** Extra ppx derivers to append to the [[@@deriving ...]] of generated types, split
        by kind of generated type (see {!Extra_derivers}). *)
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
