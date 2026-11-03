open! Core
open! Async
open Openapi_codegen_ir
open Openapi_codegen_ir.Typify
open Openapi_spec.Types
open! To_jingoo.Export
open Jingoo
open Embedded_strings

let jbuild_corrected_file = "dune.corrected"

let rec append_underscores_until_no_conflict names name =
  if Set.mem names name
  then append_underscores_until_no_conflict names (name ^ "_")
  else name
;;

let escaped_ocaml_string string = {%string|"%{String.escaped string}"|}

(* Sanitizes arbitrary [description] prose from the spec so it can be embedded inside an
   odoc [(** ... *)] comment. We neutralize the comment delimiters and escape the
   odoc-significant characters ([\], [{], [}], [[], []], [@]) so the text renders verbatim
   and can never break the generated interface, which is now compiled and style-checked. *)
let escape_for_odoc text =
  text
  |> String.substr_replace_all ~pattern:"*)" ~with_:"* )"
  |> String.substr_replace_all ~pattern:"(*" ~with_:"( *"
  |> String.concat_map ~f:(fun char ->
    match char with
    | '\\' -> {|\\|}
    | '{' -> {|\{|}
    | '}' -> {|\}|}
    | '[' -> {|\[|}
    | ']' -> {|\]|}
    | '@' -> {|\@|}
    | char -> String.of_char char)
;;

(* The body of an odoc comment for [description], or [None] when descriptions are not
   being propagated or there is nothing to say. The result does not include the
   surrounding [(** ... *)], which the templates add. *)
let doc_comment ~propagate_descriptions description =
  match propagate_descriptions, description with
  | false, _ | _, None -> None
  | true, Some description ->
    (match String.strip description with
     | "" -> None
     | description -> Some (escape_for_odoc description))
;;

module Models = struct
  module As_variable_name = struct
    type t = Name.t

    let to_jingoo t = [%to_jingoo: string] (Name.to_variable_name t)
  end

  module As_variant = struct
    type t = Name.t

    let to_jingoo t = [%to_jingoo: string] (Name.to_variant t)
  end

  module As_raw_string = struct
    type t = Name.t

    let to_jingoo t = [%to_jingoo: string] (Name.to_raw_string t)
  end

  module As_ocaml_string = struct
    type t = Name.t

    let to_jingoo t = Name.to_raw_string t |> escaped_ocaml_string |> [%to_jingoo: string]
  end

  module As_full_module_name = struct
    type t = Name.t

    let to_jingoo t = [%to_jingoo: string] (Name.to_module_name t ~kind:`Full)
  end

  module As_truncated_module_name = struct
    type t =
      { name : Name.t
      ; maximum_filename_length : Name.Maximum_filename_length.t
      }

    let to_jingoo { name; maximum_filename_length } =
      [%to_jingoo: string]
        (Name.to_module_name ~kind:(`Truncated maximum_filename_length) name)
    ;;
  end

  module Capitalization = struct
    include Capitalization

    let to_jingoo t = [%to_jingoo: string] (to_string t)
  end

  module Type_reference = struct
    type t = Type_description.t

    let to_jingoo t = [%to_jingoo: string] (Type_description.type_name t)
  end

  (* Renders a type reference for an interface. This differs from [Type_reference] because
     interfaces should expose the public [Jsonaf.t] rather than the internal
     [With_structural_compare.t], and should render the string-keyed map type as a plain
     association list. *)
  module Mli_type_reference = struct
    type t = Type_description.t

    let rec type_name (t : t) =
      match Type_description.Identifier.to_string t.name, t.arguments with
      | "Openapi_runtime.Jane_with_json.Assoc.M(Core.String).t", [ value ] ->
        [%string "(string * %{type_name value}) list"]
      | _ -> Type_description.type_name t
    ;;

    (* Flat renames that expose public-facing type names in interfaces. We deliberately do
       not rename the [Jsonaf_ip] / [Jsonaf_ipv4] / [Jsonaf_ipv6] runtime types: they are
       all transparent aliases of the same [Core_unix.Inet_addr.t], so collapsing them
       would erase the IPv4/IPv6 distinction the runtime type names document. *)
    let to_jingoo t =
      type_name t
      |> String.substr_replace_all
           ~pattern:"Jsonaf.With_structural_compare.t"
           ~with_:"Jsonaf.t"
      |> String.substr_replace_all
           ~pattern:"Jsonaf_kernel.With_structural_compare.t"
           ~with_:"Jsonaf.t"
      |> String.substr_replace_all
           ~pattern:"Openapi_runtime.Jane_with_json.Jsonaf_time.t"
           ~with_:"Core.Time_ns.t"
      |> String.substr_replace_all
           ~pattern:"Openapi_runtime.Jane_with_json.Jsonaf_uuid.t"
           ~with_:"Uuid.t"
      |> [%to_jingoo: string]
    ;;
  end

  module Record = struct
    module Field = struct
      type t =
        { name : As_variable_name.t
        ; api_name : As_ocaml_string.t option (* None if capitalization is enough *)
        ; optional : bool
        ; type_reference : Type_reference.t
        ; mli_type_reference : Mli_type_reference.t
        ; description : string option
        }
      [@@deriving to_jingoo]
    end

    module Additional_properties = struct
      type t =
        { field_name : string
        ; type_reference : Type_reference.t
        ; mli_type_reference : Mli_type_reference.t
        }
      [@@deriving to_jingoo]
    end

    type t =
      { fields : Field.t list
      ; capitalization : Capitalization.t option
      ; raise_on_optional_null : bool
      ; add_sexp_option_annotations : bool
      ; additional_properties : Additional_properties.t option
      ; allow_extra_fields : bool
      ; extra_derivers : string list
      ; description : string option
      }
    [@@deriving to_jingoo]
  end

  module String_variant = struct
    module Branch = struct
      type t =
        { name : As_variant.t
        ; api_name : As_ocaml_string.t option
        }
      [@@deriving to_jingoo]
    end

    type t =
      { module_name : As_full_module_name.t
      ; variants : Branch.t list
      ; capitalization : Capitalization.t option
      ; unknown_variant : string option
      ; extra_derivers : string list
      ; description : string option
      }
    [@@deriving to_jingoo]
  end

  module Object_variant = struct
    module Branch = struct
      type t =
        { name : As_variant.t
        ; api_name : As_ocaml_string.t
        ; type_reference : Type_reference.t
        ; mli_type_reference : Mli_type_reference.t
        ; description : string option
        }
      [@@deriving to_jingoo]
    end

    type t =
      { module_name : As_full_module_name.t
      ; variants : Branch.t list
      ; tag : As_ocaml_string.t
      ; extra_derivers : string list
      ; description : string option
      }
    [@@deriving to_jingoo]
  end

  module Transparent_variant = struct
    module Branch = struct
      type t =
        { name : As_variant.t
        ; type_reference : Type_reference.t
        ; mli_type_reference : Mli_type_reference.t
        ; description : string option
        }
      [@@deriving to_jingoo]
    end

    type t =
      { module_name : As_full_module_name.t
      ; variants : Branch.t list
      ; to_stringable : bool
      ; needs_string_primitives : bool
      ; extra_derivers : string list
      ; description : string option
      }
    [@@deriving to_jingoo]
  end

  module Http_method = struct
    type t = Httpaf.Method.t

    let to_string = function
      | `GET -> "`GET"
      | `CONNECT -> "`CONNECT"
      | `DELETE -> "`DELETE"
      | `HEAD -> "`HEAD"
      | `OPTIONS -> "`OPTIONS"
      | `Other str -> [%string {|(`Other "%{str}")|}]
      | `POST -> "`POST"
      | `PUT -> "`PUT"
      | `TRACE -> "`TRACE"
    ;;

    let to_jingoo t = [%to_jingoo: string] (to_string t)
  end

  module Path = struct
    module Part = struct
      type t =
        | Constant of string
        | Parameter of
            { name : Name.t
            ; type_reference : Type_description.t
            }
    end

    type t = Part.t list

    let to_jingoo (t : t) =
      List.map t ~f:(function
        | Constant constant -> [%string "/%{constant}"]
        | Parameter
            { name
            ; type_reference =
                { name = { namespace = None; name = "string" }; arguments = []; _ }
            } -> [%string "/{%{Name.to_variable_name name}}"]
        | Parameter { name; type_reference } ->
          [%string
            "/{%{Name.to_variable_name name} : %{Type_description.type_name \
             type_reference}}"])
      |> String.concat
      |> escaped_ocaml_string
      |> [%to_jingoo: string]
    ;;

    let needs_string_primitives (t : t) =
      List.exists t ~f:(function
        | Constant _ -> false
        | Parameter { type_reference; name = _ } -> type_reference.needs_string_primitives)
    ;;
  end

  module To_query_parameter = struct
    module Singleton_type = struct
      type t =
        { type_description : Type_description.t
        ; nullable : bool
        }
    end

    module Structure_parameters = struct
      type t =
        { style : Parameter.Query_style.t
        ; explode : bool
        }
    end

    type t =
      | Singleton of Singleton_type.t
      | Array of
          { element_type : Singleton_type.t
          ; structure_parameters : Structure_parameters.t
          }
      | Map of
          { element_type : Singleton_type.t
          ; structure_parameters : Structure_parameters.t
          }
      | Object of Structure_parameters.t

    let needs_string_primitives = function
      | Singleton element_type
      | Array { element_type; structure_parameters = _ }
      | Map { element_type; structure_parameters = _ } ->
        element_type.type_description.needs_string_primitives
      | Object _ -> false
    ;;

    let singleton_type_to_string { Singleton_type.type_description; nullable } =
      let to_string =
        [%string "[%to_string: %{Type_description.type_name type_description}]"]
      in
      if nullable
      then [%string {|(Openapi_runtime.Query_parameters.make_nullable %{to_string})|}]
      else to_string
    ;;

    let structure_parameters_to_string { Structure_parameters.style; explode } =
      let style =
        match (style : Parameter.Query_style.t) with
        | Form -> None
        | Space_delimited -> Some "~style:Space_delimited"
        | Pipe_delimited -> Some "~style:Pipe_delimited"
        | Deep_object -> Some "~style:Deep_object"
      in
      let explode = if explode then None else Some "~explode:false" in
      List.filter_opt [ style; explode ] |> String.concat ~sep:" "
    ;;

    let to_string = function
      | Singleton singleton_type ->
        let to_string = singleton_type_to_string singleton_type in
        [%string {|Openapi_runtime.Query_parameters.singleton %{to_string}|}]
      | Array { element_type; structure_parameters } ->
        let to_string = singleton_type_to_string element_type in
        let parameters = structure_parameters_to_string structure_parameters in
        [%string {|Openapi_runtime.Query_parameters.array %{parameters} %{to_string}|}]
      | Map { element_type; structure_parameters } ->
        let to_string = singleton_type_to_string element_type in
        let parameters = structure_parameters_to_string structure_parameters in
        [%string {|Openapi_runtime.Query_parameters.map %{parameters} %{to_string}|}]
      | Object structure_parameters ->
        let parameters = structure_parameters_to_string structure_parameters in
        [%string {|Openapi_runtime.Query_parameters.object_ %{parameters} ()|}]
    ;;

    let singleton_value_type ({ type_description; nullable } : Singleton_type.t) =
      if nullable then Type_description.option type_description else type_description
    ;;

    (* The OCaml type of the value accepted for this query parameter, used to render
       interface signatures. *)
    let value_type = function
      | Singleton singleton_type -> singleton_value_type singleton_type
      | Array { element_type; structure_parameters = _ } ->
        Type_description.list (singleton_value_type element_type)
      | Map { element_type; structure_parameters = _ } ->
        Type_description.string_assoc (singleton_value_type element_type)
      | Object (_ : Structure_parameters.t) ->
        Type_description.string_assoc Type_description.Presets.jsonaf
    ;;

    let to_jingoo t = [%to_jingoo: string] (to_string t)
  end

  module Path_parameter = struct
    type t =
      { name : As_variable_name.t
      ; mli_type_reference : Mli_type_reference.t
      }
    [@@deriving to_jingoo]
  end

  module Query_parameter = struct
    type t =
      { name : As_variable_name.t
      ; api_name : As_raw_string.t
      ; is_required : bool
      ; to_query_parameter : To_query_parameter.t
      ; mli_type_reference : Mli_type_reference.t
      }
    [@@deriving to_jingoo]
  end

  module Operation = struct
    module Id = struct
      type t =
        | Normalized of As_variable_name.t
        | As_is of string

      let to_jingoo = function
        | Normalized name -> [%to_jingoo: As_variable_name.t] name
        | As_is name -> [%to_jingoo: string] name
      ;;
    end

    type t =
      { operation_id : Id.t
      ; doc_comment : string option
      ; operation_path : Path.t
      ; has_body : bool
      ; body_json_type : Type_reference.t option
      ; body_mli_type : Mli_type_reference.t
      ; endpoint_method : Http_method.t
      ; has_raw_response : bool
      ; response_json_type : Type_reference.t option
      ; response_mli_type : Mli_type_reference.t
      ; path_parameters : Path_parameter.t list
      ; query_parameters : Query_parameter.t list
      }
    [@@deriving to_jingoo]

    let needs_string_primitives t =
      Path.needs_string_primitives t.operation_path
      || List.exists t.query_parameters ~f:(fun parameter ->
        To_query_parameter.needs_string_primitives parameter.to_query_parameter)
    ;;
  end

  module Operation_definition = struct
    type t =
      { operations : Operation.t list
      ; needs_string_primitives : bool
      }
    [@@deriving to_jingoo]
  end

  module Module_alias = struct
    type t =
      { nice_name : As_full_module_name.t
      ; real_name : As_truncated_module_name.t
      }
    [@@deriving to_jingoo]
  end

  module Types_index = struct
    type t = { types : Module_alias.t list } [@@deriving to_jingoo]
  end

  module Archive = struct
    type t =
      { name : string
      ; files : string
      }
    [@@deriving to_jingoo]
  end

  module Jbuild = struct
    type t =
      { name : string
      ; spec : string
      ; targets : string
      ; archive : Archive.t option
      ; generator_flags : string
      }
    [@@deriving to_jingoo]
  end

  module Type_definition_wrapper = struct
    type t = { module_ : string }

    let to_jingoo { module_ } = Jg_types.Tobj [ "module", [%to_jingoo: string] module_ ]
  end
end

let models_of_obj_exn = function
  | Jg_types.Tobj obj -> obj
  | _ -> failwith "models_of_obj_exn: not an object"
;;

(* [used_as] describes how the name will be emitted, which determines the OCaml identifier
   whose capitalization we are reasoning about. Variant constructors are capitalized, so
   their snake-case form is never keyword-escaped; record fields (and other lowercase
   identifiers) are escaped when they collide with a keyword, e.g. [type] -> [type_]. *)
let capitalization_applies ?(used_as = `Variable) capitalization name =
  let snake_case =
    match used_as with
    | `Variant -> String.uncapitalize (Name.to_variant name)
    | `Variable -> Name.to_variable_name name
  in
  let capitalization_gets_back_the_raw_string =
    String.equal
      (Name.to_raw_string name)
      (Capitalization.apply_to_snake_case capitalization snake_case)
  in
  (* Sometimes, capitalization can result in a bit of a confusing output:
     [apply_to_snake_case] uses underscore as a word separator, and replaces the
     underscores with a capitalization-specific separator in the final result, or drops
     them if the capitalization doesn't have a separator (for example camelCase or
     PascalCase). If the value has trailing underscores (perhaps because it's a reserved
     keyword) but the capitalization doesn't have a separator, the trailing underscores
     are silently dropped, which can be confusing to read, so we explicitly don't allow
     this case.
  *)
  let trailing_underscores_are_silently_dropped =
    String.is_suffix ~suffix:"_" snake_case
    && Option.is_none (Capitalization.separator capitalization)
  in
  capitalization_gets_back_the_raw_string && not trailing_underscores_are_silently_dropped
;;

(* If we were to try to do this super properly, we would check to see if all the options
   are a single word, and use single word capitalization if that's the case (so lowercase
   instead of camelCase) because we don't know what multiword capitalization would
   actually look like.

   However, these types are auto-generated, so we can just change the picked
   capitalization later, so this doesn't matter as much. *)
let pick_capitalization ?used_as names =
  let most_likely_capitalizations =
    List.map Capitalization.all ~f:(fun capitalization ->
      List.count names ~f:(capitalization_applies ?used_as capitalization), capitalization)
    |> List.sort ~compare:(Comparable.lift ~f:fst Int.descending)
  in
  match most_likely_capitalizations with
  | [ (_, capitalization) ] -> Some capitalization
  | (top_count, capitalization) :: _
    when top_count * 2 > List.length names (* Has to be the strict majority of cases *) ->
    Some capitalization
  | _ -> None
;;

let optional_capitalization_applies ?used_as capitalization name =
  Option.exists capitalization ~f:(fun capitalization ->
    capitalization_applies ?used_as capitalization name)
;;

let rec type_reference ~type_id ~type_space ~maximum_filename_length =
  (let%map.Option type_ = Type_space.type_of_id ~type_id type_space in
   match Type.structure type_ with
   | Record _ | String_variant _ | Object_variant _ | Transparent_variant _ ->
     Type_description.generated (Type.name type_) ~maximum_filename_length
   | List type_id ->
     Type_description.list (type_reference ~type_id ~type_space ~maximum_filename_length)
   | Nullable type_id ->
     Type_description.option
       (type_reference ~type_id ~type_space ~maximum_filename_length)
   | Map type_id ->
     Type_description.string_assoc
       (type_reference ~type_id ~type_space ~maximum_filename_length)
   | Existing_type type_description -> type_description)
  |> Option.value ~default:Type_description.Presets.jsonaf
;;

let rec type_to_stringable ~type_id ~type_space =
  Option.exists (Type_space.type_of_id ~type_id type_space) ~f:(fun type_ ->
    match Type.structure type_ with
    | Record _ | Object_variant _ | List _ | Map _ | Nullable _ -> false
    | Transparent_variant variant ->
      List.for_all variant ~f:(fun (type_id, _) ->
        type_to_stringable ~type_id ~type_space)
    | Existing_type _ | String_variant _ -> true)
;;

let env = { Jg_types.std_env with autoescape = false; strict_mode = true }

let record_type_definition
  ~type_space
  ~raise_on_optional_null
  ~properties
  ~additional_properties
  ~maximum_filename_length
  ~add_sexp_option_annotations
  ~extra_derivers
  ~propagate_descriptions
  ~description
  =
  let capitalization = Map.keys properties |> pick_capitalization in
  let fields =
    Map.mapi
      properties
      ~f:(fun ~key:name ~data:{ Property.type_id; necessity; description } ->
        let api_name =
          if optional_capitalization_applies capitalization name then None else Some name
        in
        let type_reference =
          type_reference ~type_id ~type_space ~maximum_filename_length
        in
        let optional, type_reference =
          match necessity with
          | `Required -> false, type_reference
          | `Optional -> true, Type_description.option type_reference
        in
        { Models.Record.Field.name
        ; api_name
        ; optional
        ; type_reference
        ; mli_type_reference = type_reference
        ; description = doc_comment ~propagate_descriptions description
        })
    |> Map.data
  in
  let additional_properties, allow_extra_fields =
    match additional_properties with
    | `Explicit type_id ->
      let type_reference = type_reference ~type_id ~type_space ~maximum_filename_length in
      let existing_properties =
        List.map fields ~f:(fun field -> Name.to_variable_name field.name)
        |> String.Set.of_list
      in
      let additional_properties =
        { Models.Record.Additional_properties.type_reference
        ; mli_type_reference = type_reference
        ; field_name =
            append_underscores_until_no_conflict
              existing_properties
              "additional_properties"
        }
      in
      Some additional_properties, false
    | `Allowed -> None, true
    | `Not_allowed -> None, false
  in
  { Models.Record.fields
  ; capitalization
  ; raise_on_optional_null
  ; additional_properties
  ; allow_extra_fields
  ; add_sexp_option_annotations
  ; extra_derivers
  ; description = doc_comment ~propagate_descriptions description
  }
;;

let string_variant_type_definition
  ~module_name
  ~branches
  ~include_unknown_fallback_for_enums
  ~extra_derivers
  ~propagate_descriptions
  ~description
  =
  let capitalization = pick_capitalization ~used_as:`Variant branches in
  let variants =
    List.map branches ~f:(fun name ->
      let api_name =
        if optional_capitalization_applies ~used_as:`Variant capitalization name
        then None
        else Some name
      in
      { Models.String_variant.Branch.api_name; name })
  in
  let unknown_variant =
    if include_unknown_fallback_for_enums
    then (
      let existing_names = List.map branches ~f:Name.to_variant |> String.Set.of_list in
      Some (append_underscores_until_no_conflict existing_names "Unknown"))
    else None
  in
  { Models.String_variant.module_name
  ; variants
  ; capitalization
  ; unknown_variant
  ; extra_derivers
  ; description = doc_comment ~propagate_descriptions description
  }
;;

(* A variant constructor's doc comment is taken from the [description] of the type it
   wraps, since [oneOf] members are usually [$ref]s to documented component schemas. *)
let branch_description ~type_space ~propagate_descriptions ~type_id =
  Type_space.type_of_id ~type_id type_space
  |> Option.bind ~f:Type.description
  |> doc_comment ~propagate_descriptions
;;

let object_variant_type_definition
  ~type_space
  ~module_name
  ~branches
  ~maximum_filename_length
  ~tag
  ~extra_derivers
  ~propagate_descriptions
  ~description
  =
  let variants =
    List.map branches ~f:(fun (type_id, name) ->
      let type_reference = type_reference ~type_id ~type_space ~maximum_filename_length in
      { Models.Object_variant.Branch.api_name = name
      ; type_reference
      ; mli_type_reference = type_reference
      ; name
      ; description = branch_description ~type_space ~propagate_descriptions ~type_id
      })
  in
  { Models.Object_variant.module_name
  ; tag = Name.of_raw_string tag
  ; variants
  ; extra_derivers
  ; description = doc_comment ~propagate_descriptions description
  }
;;

let transparent_variant_type_definition
  ~type_id
  ~type_space
  ~module_name
  ~branches
  ~maximum_filename_length
  ~extra_derivers
  ~propagate_descriptions
  ~description
  =
  let variants =
    List.map branches ~f:(fun (type_id, name) ->
      let type_reference = type_reference ~type_id ~type_space ~maximum_filename_length in
      { Models.Transparent_variant.Branch.type_reference
      ; mli_type_reference = type_reference
      ; name
      ; description = branch_description ~type_space ~propagate_descriptions ~type_id
      })
  in
  let to_stringable = type_to_stringable ~type_id ~type_space in
  let needs_string_primitives =
    to_stringable
    && List.exists variants ~f:(fun branch ->
      branch.type_reference.needs_string_primitives)
  in
  { Models.Transparent_variant.module_name
  ; variants
  ; to_stringable = type_to_stringable ~type_id ~type_space
  ; needs_string_primitives
  ; extra_derivers
  ; description = doc_comment ~propagate_descriptions description
  }
;;

let type_definition_template ~for_mli type_structure =
  match type_structure, for_mli with
  | `Record, false -> record_definition_dot_jingoo
  | `Record, true -> record_definition_dot_mli_dot_jingoo
  | `String_variant, false -> variant_definition_dot_jingoo
  | `String_variant, true -> variant_definition_dot_mli_dot_jingoo
  | `Object_variant, false -> variant_definition_object_dot_jingoo
  | `Object_variant, true -> variant_definition_object_dot_mli_dot_jingoo
  | `Transparent_variant, false -> variant_definition_transparent_dot_jingoo
  | `Transparent_variant, true -> variant_definition_transparent_dot_mli_dot_jingoo
;;

let type_definition_for
  ~for_mli
  ~type_id
  ~type_space
  ~code_gen_config:
    { Config.Code_gen.include_unknown_fallback_for_enums
    ; raise_on_optional_null
    ; maximum_filename_length
    ; normalize_operation_ids = _
    ; add_sexp_option_annotations
    ; propagate_descriptions
    ; extra_derivers =
        { record = extra_record_derivers
        ; string_variant = extra_string_variant_derivers
        ; object_variant = extra_object_variant_derivers
        ; transparent_variant = extra_transparent_variant_derivers
        }
    }
  =
  let maximum_filename_length =
    Option.value maximum_filename_length ~default:Name.Maximum_filename_length.default
  in
  let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
  let description = Type.description type_ in
  match Type.structure type_ with
  | Record { properties; additional_properties } ->
    let models =
      record_type_definition
        ~type_space
        ~properties
        ~raise_on_optional_null
        ~maximum_filename_length
        ~additional_properties
        ~add_sexp_option_annotations
        ~extra_derivers:extra_record_derivers
        ~propagate_descriptions
        ~description
      |> [%to_jingoo: Models.Record.t]
      |> models_of_obj_exn
    in
    Some
      (Jg_template.from_string ~env ~models (type_definition_template ~for_mli `Record))
  | String_variant branches ->
    let models =
      string_variant_type_definition
        ~branches
        ~module_name:(Type.name type_)
        ~include_unknown_fallback_for_enums
        ~extra_derivers:extra_string_variant_derivers
        ~propagate_descriptions
        ~description
      |> [%to_jingoo: Models.String_variant.t]
      |> models_of_obj_exn
    in
    Some
      (Jg_template.from_string
         ~env
         ~models
         (type_definition_template ~for_mli `String_variant))
  | Object_variant (branches, tag) ->
    let models =
      object_variant_type_definition
        ~maximum_filename_length
        ~type_space
        ~branches
        ~module_name:(Type.name type_)
        ~tag
        ~extra_derivers:extra_object_variant_derivers
        ~propagate_descriptions
        ~description
      |> [%to_jingoo: Models.Object_variant.t]
      |> models_of_obj_exn
    in
    Some
      (Jg_template.from_string
         ~env
         ~models
         (type_definition_template ~for_mli `Object_variant))
  | Transparent_variant branches ->
    let models =
      transparent_variant_type_definition
        ~maximum_filename_length
        ~type_space
        ~branches
        ~module_name:(Type.name type_)
        ~type_id
        ~extra_derivers:extra_transparent_variant_derivers
        ~propagate_descriptions
        ~description
      |> [%to_jingoo: Models.Transparent_variant.t]
      |> models_of_obj_exn
    in
    Some
      (Jg_template.from_string
         ~env
         ~models
         (type_definition_template ~for_mli `Transparent_variant))
  | Existing_type _ | List _ | Nullable _ | Map _ -> None
;;

let type_definition ~type_id ~type_space ~code_gen_config =
  type_definition_for ~for_mli:false ~type_id ~type_space ~code_gen_config
;;

let get_type_names ~type_space ~maximum_filename_length =
  let type_space_map = Type_space.to_map type_space in
  Map.to_alist type_space_map
  |> List.filter_map ~f:(fun (type_id, type_) ->
    let%map.Option _ =
      type_definition
        ~type_id
        ~type_space
        ~code_gen_config:
          { raise_on_optional_null = true
          ; include_unknown_fallback_for_enums = false
          ; maximum_filename_length = None
          ; normalize_operation_ids = true
          ; add_sexp_option_annotations = false
          ; propagate_descriptions = false
          ; extra_derivers = Config.Code_gen.Extra_derivers.empty
          }
      (* None of this matters, we just want to see if the type generates a new file *)
    in
    Type.name type_)
  |> List.dedup_and_sort ~compare:Name.compare
  |> List.map ~f:(fun name ->
    { Models.Module_alias.nice_name = name
    ; real_name = { name; maximum_filename_length }
    })
;;

let make_type_files ~for_mli ~type_space ~code_gen_config =
  let type_space_map = Type_space.to_map type_space in
  Map.to_alist type_space_map
  |> List.map ~f:(fun (type_id, type_) -> type_id, Type.name type_)
  |> List.map ~f:(fun (type_id, type_name) ->
    type_definition_for ~for_mli ~type_id ~type_space ~code_gen_config, type_name)
  |> List.filter_map ~f:(fun (type_definition, type_name) ->
    match type_definition with
    | None -> None
    | Some type_definition -> Some (type_definition, type_name))
  |> List.map ~f:(fun (module_definition, type_name) ->
    let models =
      { Models.Type_definition_wrapper.module_ = module_definition }
      |> [%to_jingoo: Models.Type_definition_wrapper.t]
      |> models_of_obj_exn
    in
    let template =
      if for_mli
      then type_definition_dot_mli_dot_jingoo
      else type_definition_dot_ml_dot_jingoo
    in
    let definition_str = Jg_template.from_string ~env ~models template in
    definition_str, type_name)
;;

let make_type_mls ~type_space ~code_gen_config =
  make_type_files ~for_mli:false ~type_space ~code_gen_config
;;

let make_type_mlis ~type_space ~code_gen_config =
  make_type_files ~for_mli:true ~type_space ~code_gen_config
;;

let generate_to_query_parameter
  ~type_space
  operation_parameter
  ~maximum_filename_length
  ~style
  =
  match Operation_parameter.type_ operation_parameter with
  | Raw_body -> None
  | Type type_id ->
    let singleton_type type_id =
      if type_to_stringable ~type_id ~type_space
      then (
        let type_description =
          type_reference ~type_id ~type_space ~maximum_filename_length
        in
        Some
          { Models.To_query_parameter.Singleton_type.type_description; nullable = false })
      else None
    in
    let structure_parameters =
      { Models.To_query_parameter.Structure_parameters.style
      ; explode =
          (match Operation_parameter.explode operation_parameter with
           | None | Some true -> true
           | Some false -> false)
      }
    in
    let rec maybe_nullable_singleton_type type_id =
      let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
      match singleton_type type_id, Type.structure type_ with
      | Some singleton_type, _ -> Some singleton_type
      | None, Nullable type_id ->
        let%map.Option singleton_type = maybe_nullable_singleton_type type_id in
        { singleton_type with Models.To_query_parameter.Singleton_type.nullable = true }
      | _ -> None
    in
    let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
    (match maybe_nullable_singleton_type type_id, Type.structure type_ with
     | Some singleton_type, _ -> Some (Models.To_query_parameter.Singleton singleton_type)
     | _, List type_id ->
       let%map.Option element_type = maybe_nullable_singleton_type type_id in
       Models.To_query_parameter.Array { element_type; structure_parameters }
     | _, Map type_id ->
       let%map.Option element_type = maybe_nullable_singleton_type type_id in
       Models.To_query_parameter.Map { element_type; structure_parameters }
     | _, Record _ -> Some (Models.To_query_parameter.Object structure_parameters)
     | _ -> None)
;;

let type_description_of_string string =
  Type_description.create (Type_description.Identifier.of_string string)
;;

let operation_model
  ~operation
  ~type_space
  ~maximum_filename_length
  ~normalize_operation_ids
  ~propagate_descriptions
  =
  let operation_id = Operation_method.operation_id operation in
  let operation_parameters = Operation_method.parameters operation in
  let operation_path =
    let path_parameter_types_by_name =
      List.filter_map operation_parameters ~f:(fun parameter ->
        match parameter.kind, parameter.type_ with
        | Path, Type type_id ->
          if type_to_stringable ~type_id ~type_space
          then
            Some
              ( Name.to_raw_string parameter.name
              , type_reference ~type_id ~type_space ~maximum_filename_length )
          else None
        | Path, Raw_body | (Query _ | Header _ | Body _), _ -> None)
      |> String.Map.of_alist_multi
    in
    List.map (Operation_method.path operation) ~f:(function
      | Constant constant -> Models.Path.Part.Constant constant
      | Parameter parameter ->
        let type_reference =
          match Map.find_multi path_parameter_types_by_name parameter with
          | [] | _ :: _ :: _ (* Ambiguous *) -> Type_description.Presets.string
          | [ type_description ] -> type_description
        in
        Parameter { name = Name.of_raw_string parameter; type_reference })
  in
  let operation_method = Operation_method.http_method operation in
  let body =
    operation_parameters
    |> List.map ~f:(fun param ->
      match Operation_parameter.kind param with
      | Body _ -> Some param
      | _ -> None)
    |> List.filter_opt
    |> List.hd
  in
  let path_parameters =
    List.filter_map operation_path ~f:(function
      | Constant _ -> None
      | Parameter { name; type_reference } ->
        Some { Models.Path_parameter.name; mli_type_reference = type_reference })
  in
  let query_parameters =
    operation_parameters
    |> List.filter_map ~f:(fun param ->
      let%bind.Option is_required, style =
        match Operation_parameter.kind param with
        | Path | Header _ | Body _ -> None
        | Query { required; style } -> Some (required, style)
      in
      let%map.Option to_query_parameter =
        generate_to_query_parameter ~type_space param ~maximum_filename_length ~style
      in
      { Models.Query_parameter.name = Operation_parameter.name param
      ; api_name = Operation_parameter.name param
      ; is_required
      ; to_query_parameter
      ; mli_type_reference = Models.To_query_parameter.value_type to_query_parameter
      })
  in
  let body_json_type =
    let%bind.Option body in
    match Operation_parameter.type_ body with
    | Type type_id -> Some (type_reference ~type_id ~type_space ~maximum_filename_length)
    | Raw_body -> None
  in
  let body_mli_type =
    Option.value body_json_type ~default:(type_description_of_string "string")
  in
  let operation_response =
    Operation_response.get_success_response_for_operation
      (Operation_method.responses operation)
    |> Option.value_exn
  in
  let response_json_type, has_raw_response =
    match Operation_response.type_id operation_response with
    | Resolved type_id ->
      Some (type_reference ~type_id ~type_space ~maximum_filename_length), false
    | Raw -> None, true
    | _ -> None, false
  in
  let response_mli_type =
    match response_json_type, has_raw_response with
    | Some response_json_type, false -> response_json_type
    | None, true ->
      type_description_of_string "Openapi_runtime.Client.Content_type_and_bytes.t"
    | Some (_ : Type_description.t), true ->
      raise_s [%message "OpenAPI response cannot be both JSON and raw"]
    | None, false -> type_description_of_string "unit"
  in
  let doc_comment =
    let summary =
      doc_comment
        ~propagate_descriptions
        (match Operation_method.description operation with
         | Some _ as description -> description
         | None -> Operation_method.summary operation)
    in
    let signature_param_labels =
      List.concat
        [ List.map path_parameters ~f:(fun param -> Name.to_variable_name param.name)
        ; List.map query_parameters ~f:(fun param -> Name.to_variable_name param.name)
        ; (match body with
           | None -> []
           | Some (_ : Operation_parameter.t) -> [ "body" ])
        ]
      |> String.Set.of_list
    in
    let param_lines =
      List.filter_map operation_parameters ~f:(fun param ->
        let label =
          match Operation_parameter.kind param with
          | Header _ -> None
          | Body _ -> Some "body"
          | Path | Query _ ->
            Some (Name.to_variable_name (Operation_parameter.name param))
        in
        let%bind.Option label in
        let%bind.Option () = Option.some_if (Set.mem signature_param_labels label) () in
        let%map.Option description =
          doc_comment ~propagate_descriptions (Operation_parameter.description param)
        in
        let description =
          String.split_lines description
          |> List.map ~f:String.strip
          |> String.concat ~sep:" "
        in
        [%string "@param %{label} %{description}"])
    in
    match summary, param_lines with
    | None, [] -> None
    | _ -> Some (String.concat ~sep:"\n\n" (Option.to_list summary @ param_lines))
  in
  let operation_id : Models.Operation.Id.t =
    if normalize_operation_ids
    then Normalized (Name.of_raw_string operation_id)
    else As_is operation_id
  in
  { Models.Operation.operation_id
  ; doc_comment
  ; operation_path
  ; has_body = Option.is_some body
  ; body_json_type
  ; body_mli_type
  ; endpoint_method = operation_method
  ; has_raw_response
  ; response_json_type
  ; response_mli_type
  ; path_parameters
  ; query_parameters
  }
;;

let make_types ~for_mli ~types =
  let models = [%to_jingoo: Models.Types_index.t] { types } |> models_of_obj_exn in
  let template = if for_mli then types_dot_mli_dot_jingoo else types_dot_ml_dot_jingoo in
  Jg_template.from_string ~env ~models template
;;

let make_types_ml = make_types ~for_mli:false
let make_types_mli = make_types ~for_mli:true

let make_operation_definition
  ~for_mli
  ~operation_list
  ~type_space
  ~maximum_filename_length
  ~normalize_operation_ids
  ~propagate_descriptions
  =
  let operations =
    List.map operation_list ~f:(fun operation ->
      operation_model
        ~operation
        ~type_space
        ~maximum_filename_length
        ~normalize_operation_ids
        ~propagate_descriptions)
  in
  let models =
    { Models.Operation_definition.operations
    ; needs_string_primitives =
        List.exists operations ~f:Models.Operation.needs_string_primitives
    }
    |> [%to_jingoo: Models.Operation_definition.t]
    |> models_of_obj_exn
  in
  let template =
    if for_mli
    then operation_definition_dot_mli_dot_jingoo
    else operation_definition_dot_jingoo
  in
  Jg_template.from_string ~env ~models template
;;

(* The [.ml] never carries doc comments; descriptions are an interface-only concern. *)
let make_operation_definition_ml =
  make_operation_definition ~for_mli:false ~propagate_descriptions:false
;;

let make_operation_definition_mli = make_operation_definition ~for_mli:true

let make_jbuild ~(config : Config.t) ~paths =
  let paths = Set.to_list paths in
  let archive, targets =
    match config.generated_files_archive with
    | None -> None, String.concat_lines (jbuild_corrected_file :: paths)
    | Some archive ->
      ( Some { Models.Archive.name = archive; files = String.concat_lines paths }
      , String.concat_lines [ jbuild_corrected_file; archive ] )
  in
  let models =
    [%to_jingoo: Models.Jbuild.t]
      { name = config.name
      ; spec = config.spec_file
      ; targets
      ; archive
      ; generator_flags =
          String.concat
            ~sep:" "
            (Roundtrippable_command_param.command_args
               Config.roundtrippable_command_param
               config)
      }
    |> models_of_obj_exn
  in
  Jg_template.from_string ~env ~models dune_dot_jingoo
;;

let make_operation_method_lists ~type_space ~paths ~components =
  let new_map, type_space =
    Map.fold
      paths
      ~init:(Name.Map.empty, type_space)
      ~f:(fun ~key ~data:_ (new_map, type_space) ->
        let path = key in
        let path_item = Map.find_exn paths path in
        let operations = Path_item.all_operations path_item in
        let type_space, operation_methods =
          List.fold_map
            operations
            ~init:type_space
            ~f:(fun type_space (http_method, op) ->
              Openapi_codegen_ir.Operation_method.of_operation
                ~path
                ~http_method
                ~path_parameters:path_item.parameters
                ~components
                ~type_space
                op
              |> Option.value_map
                   ~default:(type_space, None)
                   ~f:(fun (operation_method, type_space) ->
                     type_space, Some operation_method))
        in
        let operation_methods = List.filter_opt operation_methods in
        let key = Name.of_operation_path key in
        Map.add_exn new_map ~key ~data:operation_methods, type_space)
  in
  Map.to_alist new_map, type_space
;;

let make_operation_method_list ~type_space ~paths ~components =
  let operation_lists, type_space =
    make_operation_method_lists ~type_space ~paths ~components
  in
  List.concat_map operation_lists ~f:snd, type_space
;;

(* The library's main module exposes only [Types], [Params], and [Operations]. The
   individual type and endpoint modules are reached through those, so we no longer alias
   them here. [Types] is [include]d so that types are not prefixed for clients. *)
let make_module_aliases_ml () =
  Jg_template.from_string ~env ~models:[] module_aliases_dot_ml_dot_jingoo
;;

let non_colliding_operation_name names proposal ~maximum_filename_length =
  let rec non_colliding_operation_name try_ =
    let new_name =
      String.concat
        [ Name.to_raw_string proposal
        ; "_operations"
        ; (if try_ = 0 then "" else Int.to_string try_)
        ]
      |> Name.of_operation_path
    in
    if List.exists names ~f:(Name.filenames_equal new_name ~maximum_filename_length)
    then non_colliding_operation_name (try_ + 1)
    else new_name
  in
  if List.exists names ~f:(Name.filenames_equal proposal ~maximum_filename_length)
  then non_colliding_operation_name 0
  else proposal
;;

(* The constant segments of a path, as module names. Parameter segments are dropped: they
   become function arguments rather than module levels (e.g. [/store/order/{orderId}] has
   segments [Store; Order]). These determine where the endpoint attaches in the operation
   tree. *)
let path_segments path =
  Name.to_raw_string path
  |> Openapi_runtime.Path_template.of_template_string
  |> Option.value ~default:[]
  |> List.filter_map ~f:(function
    | Openapi_runtime.Path_template.Path_component.Constant "" -> None
    | Constant segment -> Some (Name.of_raw_string segment)
    | Parameter _ -> None)
;;

(* Each endpoint gets a [real_name]: the file (and file-module) name. It must be unique
   across both the endpoint files and the generated type files, so it is suffixed with
   [_operations] (and possibly a number) on collision. The endpoint's path segments
   determine where it is exposed in the operation tree built by [operations.ml] /
   [params.ml]. *)
let disambiguate_names (operation_lists, type_space) ~maximum_filename_length =
  let type_names =
    Type_space.to_map type_space |> Map.data |> List.map ~f:Typify.Type.name
  in
  let operation_names = List.map operation_lists ~f:fst in
  let _all_names, new_operation_lists =
    List.fold
      operation_lists
      ~init:(operation_names, [])
      ~f:(fun (operation_names, new_operation_lists) (path, operations) ->
        let operation_names_without_current =
          List.filter operation_names ~f:(Fn.non (Name.equal path))
        in
        let real_name =
          non_colliding_operation_name
            (type_names @ operation_names_without_current)
            path
            ~maximum_filename_length
        in
        ( real_name :: operation_names_without_current
        , (real_name, path_segments path, operations) :: new_operation_lists ))
  in
  (* preserve original order *)
  List.rev new_operation_lists, type_space
;;

(* A tree mirroring the URL path hierarchy. Each constant path segment is a module level;
   parameter segments are collapsed (they become function arguments). Multiple endpoints
   can attach at the same node (e.g. [/store/order] and [/store/order/{orderId}] both
   attach at [Store.Order]), and a node can both carry operations and have children. The
   tree is rendered into [operations.ml] / [params.ml] as nested modules that expose each
   endpoint file's [Operations] / [Params] submodule. *)
module Operation_tree = struct
  type t =
    { attached : (Name.t * Operation_method.t list) list
        (* endpoint file modules whose submodule is exposed at this node *)
    ; children : t Map.M(String).t (* keyed by child segment module name *)
    }

  let empty = { attached = []; children = String.Map.empty }
  let segment_module_name segment = Name.to_module_name segment ~kind:`Full

  let rec insert t segments entry =
    match segments with
    | [] -> { t with attached = t.attached @ [ entry ] }
    | segment :: rest ->
      let key = segment_module_name segment in
      let existing = Map.find t.children key |> Option.value ~default:empty in
      let child = insert existing rest entry in
      { t with children = Map.set t.children ~key ~data:child }
  ;;

  let of_entries entries =
    List.fold entries ~init:empty ~f:(fun t (real_name, segments, operations) ->
      insert t segments (real_name, operations))
  ;;

  let operation_function_name ~normalize_operation_ids operation =
    let operation_id = Operation_method.operation_id operation in
    if normalize_operation_ids
    then Name.to_variable_name (Name.of_raw_string operation_id)
    else operation_id
  ;;

  (* Operations attaching at the same node are exposed in the same module, so their
     function names must be unique within that node. This can only be violated by a spec
     with non-unique operationIds, or by [-normalize-operation-ids] mapping two distinct
     ids to the same OCaml identifier; we fail loudly rather than silently shadow. *)
  let check_no_collisions t ~normalize_operation_ids =
    let rec check ~path { attached; children } =
      let names_with_ids =
        List.concat_map attached ~f:(fun (_real_name, operations) ->
          List.map operations ~f:(fun operation ->
            ( operation_function_name ~normalize_operation_ids operation
            , Operation_method.operation_id operation )))
      in
      (match
         List.find_a_dup names_with_ids ~compare:(Comparable.lift String.compare ~f:fst)
       with
       | None -> ()
       | Some (function_name, _) ->
         let module_path =
           if List.is_empty path then "<root>" else String.concat ~sep:"." (List.rev path)
         in
         let colliding_operation_ids =
           List.filter_map names_with_ids ~f:(fun (name, operation_id) ->
             Option.some_if (String.equal name function_name) operation_id)
         in
         raise_s
           [%message
             "Multiple operations map to the same function name within a single \
              operations module. Disambiguate the colliding operationIds in the spec."
               (module_path : string)
               (function_name : string)
               (colliding_operation_ids : string list)]);
      Map.iteri children ~f:(fun ~key ~data -> check ~path:(key :: path) data)
    in
    check ~path:[] t
  ;;

  let render t ~submodule ~for_mli ~maximum_filename_length =
    let module_ref real_name =
      [%string
        "%{Name.to_module_name real_name ~kind:(`Truncated \
         maximum_filename_length)}.%{submodule}"]
    in
    let rec render_body { attached; children } =
      let includes =
        List.map attached ~f:(fun (real_name, _operations) ->
          if for_mli
          then [%string "include module type of %{module_ref real_name}"]
          else [%string "include %{module_ref real_name}"])
      in
      let child_modules =
        Map.to_alist children
        |> List.map ~f:(fun (key, subtree) -> render_child ~key subtree)
      in
      String.concat ~sep:"\n" (includes @ child_modules)
    and render_child ~key subtree =
      match subtree.attached, Map.is_empty subtree.children with
      | [ (real_name, _operations) ], true ->
        (* A single endpoint with no children: expose it directly as an alias. *)
        [%string "module %{key} = %{module_ref real_name}"]
      | _ ->
        let body = render_body subtree in
        if for_mli
        then [%string "module %{key} : sig\n%{body}\nend"]
        else [%string "module %{key} = struct\n%{body}\nend"]
    in
    render_body t
  ;;
end

(* Renders the [operations.ml] (or [.mli]) index for a spec: builds the operation tree,
   checks for node-level name collisions (raising on failure), and renders it. Shared by
   [make_files] via [Operation_tree] and exposed for testing the tree/collision logic. *)
let make_operations_index
  ~type_space
  ~paths
  ~components
  ~maximum_filename_length
  ~normalize_operation_ids
  ~for_mli
  =
  let operation_lists, (_ : Type_space.t) =
    make_operation_method_lists ~type_space ~paths ~components
    |> disambiguate_names ~maximum_filename_length
  in
  let tree = Operation_tree.of_entries operation_lists in
  Operation_tree.check_no_collisions tree ~normalize_operation_ids;
  Operation_tree.render tree ~submodule:"Operations" ~for_mli ~maximum_filename_length
;;

let write_file ~temp_dir rel_path contents =
  let filepath = Filename.of_parts [ temp_dir; String.lowercase rel_path ] in
  Writer.with_file filepath ~f:(fun writer ->
    Writer.write writer contents;
    Deferred.return ())
;;

let make_files ~config ~api =
  let { Config.destination
      ; spec_file = _
      ; generated_files_archive
      ; name
      ; code_gen = code_gen_config
      }
    =
    config
  in
  let maximum_filename_length =
    Option.value
      code_gen_config.maximum_filename_length
      ~default:Name.Maximum_filename_length.default
  in
  let components = Open_api.components api in
  let temp_dir = Filename_unix.temp_dir "openapi_generator" name ~in_dir:destination in
  let type_space = Openapi_codegen_ir.Typify.Type_space.empty in
  let paths = Open_api.paths api in
  let operation_lists, type_space =
    make_operation_method_lists ~type_space ~paths ~components
    |> disambiguate_names ~maximum_filename_length
  in
  let normalize_operation_ids = code_gen_config.normalize_operation_ids in
  let operation_tree = Operation_tree.of_entries operation_lists in
  Operation_tree.check_no_collisions operation_tree ~normalize_operation_ids;
  let render_operation_index ~submodule ~for_mli =
    Operation_tree.render operation_tree ~submodule ~for_mli ~maximum_filename_length
  in
  let operation_files =
    List.concat_map operation_lists ~f:(fun (real_name, _segments, operation_list) ->
      let filename = Name.to_filename ~maximum_filename_length real_name in
      [ ( filename
        , make_operation_definition_ml
            ~operation_list
            ~type_space
            ~maximum_filename_length
            ~normalize_operation_ids )
      ; ( Filename.chop_extension filename ^ ".mli"
        , make_operation_definition_mli
            ~operation_list
            ~type_space
            ~maximum_filename_length
            ~normalize_operation_ids
            ~propagate_descriptions:code_gen_config.propagate_descriptions )
      ])
  in
  let type_files ~extension files =
    List.map files ~f:(fun (contents, type_name) ->
      let ml_filename = Name.to_filename type_name ~maximum_filename_length in
      Filename.chop_extension ml_filename ^ extension, contents)
  in
  let types = get_type_names ~type_space ~maximum_filename_length in
  let files_to_write =
    List.concat
      [ operation_files
      ; type_files ~extension:".ml" (make_type_mls ~type_space ~code_gen_config)
      ; type_files ~extension:".mli" (make_type_mlis ~type_space ~code_gen_config)
      ; [ "types.ml", make_types_ml ~types
        ; "types.mli", make_types_mli ~types
        ; "operations.ml", render_operation_index ~submodule:"Operations" ~for_mli:false
        ; "operations.mli", render_operation_index ~submodule:"Operations" ~for_mli:true
        ; "params.ml", render_operation_index ~submodule:"Params" ~for_mli:false
        ; "params.mli", render_operation_index ~submodule:"Params" ~for_mli:true
          (* The module aliases file is the library's main module, which is exempt from
             [mlis_are_mandatory], so it does not get an interface. *)
        ; name ^ ".ml", make_module_aliases_ml ()
        ]
      ]
  in
  let written_files = List.map files_to_write ~f:fst in
  let%bind.Deferred () =
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 16)
      files_to_write
      ~f:(fun (file, contents) -> write_file ~temp_dir file contents)
  in
  let%bind.Deferred () =
    match generated_files_archive with
    | None -> return ()
    | Some archive_name ->
      let files_in_archive = List.sort written_files ~compare:String.compare in
      Process.run_exn
        ~working_dir:temp_dir
        ~prog:"tar"
        ~args:
          ([ "--format=gnu"
           ; "--sort=name"
           ; "--mtime=@0"
           ; "--owner=0"
           ; "--group=0"
           ; "--numeric-owner"
           ; "--mode=0644"
           ; "--remove-files"
           ; "-c"
           ; "-f"
           ; archive_name
           ]
           @ files_in_archive)
        ()
      |> Deferred.ignore_m
  in
  let paths = String.Set.of_list written_files in
  let%bind.Deferred () =
    write_file ~temp_dir jbuild_corrected_file (make_jbuild ~config ~paths)
  in
  let%bind.Deferred generated_files = Sys.ls_dir temp_dir in
  let%bind.Deferred () =
    Deferred.List.iter
      generated_files
      ~how:(`Max_concurrent_jobs 16)
      ~f:(fun generated_file ->
        Unix.rename ~src:(temp_dir ^/ generated_file) ~dst:(destination ^/ generated_file))
  in
  let%bind.Deferred () = Unix.rmdir temp_dir in
  Deferred.Or_error.return ()
;;
