open! Core
open! Async

module Code_gen : sig
  (** Extra ppx deriver names appended verbatim to the [[@@deriving ...]] of each kind of
      generated type. The split is per-kind because a deriver is sometimes only valid for
      some kinds. *)
  module Extra_derivers : sig
    type t =
      { record : string list
      ; string_variant : string list
      ; object_variant : string list
      ; transparent_variant : string list
      }
    [@@deriving
      sexp, fields ~getters ~iterators:make_creator, roundtrippable_command_param]

    val empty : t
  end

  type t =
    { raise_on_optional_null : bool
    ; include_unknown_fallback_for_enums : bool
    ; maximum_filename_length : Openapi_codegen_ir.Name.Maximum_filename_length.t option
    ; normalize_operation_ids : bool
    ; add_sexp_option_annotations : bool
    ; propagate_descriptions : bool
    ; extra_derivers : Extra_derivers.t
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
