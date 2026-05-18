open! Core
open! Async
open Openapi_spec.Types
open Openapi_spec.Utils

module Type_id = struct
  module T = struct
    type t = int [@@deriving compare, sexp, hash]

    let init = 0
    let next t = t + 1
  end

  include T
  module Map = Map.Make (T)
end

module Variant_structures = struct
  type t =
    | String_enum of string Type_id.Map.t
    | Tagged_object of
        { tag : string
        ; values : string Type_id.Map.t
        }
    | Transparent_object of { names : string Type_id.Map.t }
  [@@deriving sexp]
end

module Type_structure = struct
  type t =
    | String_variant of Name.t list
    | Object_variant of (Type_id.t * Name.t) list * string
    | Transparent_variant of (Type_id.t * Name.t) list
    | Record of
        { properties : (Type_id.t * [ `Required | `Optional ]) Name.Map.t
        ; additional_properties : [ `Not_allowed | `Explicit of Type_id.t | `Allowed ]
        }
    | List of Type_id.t
    | Map of Type_id.t
    | Nullable of Type_id.t
    | Existing_type of Type_description.t
  [@@deriving sexp]
end

module Parameter_origin = struct
  type t =
    | Component of { name : string }
    | Path of
        { path : string
        ; parameter_name : string
        }
    | Operation of
        { operation_id : string
        ; parameter_name : string
        }
  [@@deriving sexp, compare]

  let raw_name = function
    | Component { name } -> name
    | Path { path; parameter_name } -> [%string "%{parameter_name}_%{path}"]
    | Operation { operation_id; parameter_name } ->
      [%string "%{parameter_name}_%{operation_id}"]
  ;;

  include Comparable.Make (struct
      type nonrec t = t [@@deriving sexp, compare]
    end)
end

module Response_origin = struct
  type t =
    | Component of { name : string }
    | Operation of
        { operation_id : string
        ; status : Operation_response_status.t
        }
  [@@deriving sexp, compare]

  let raw_name t ~success_response_for_operation =
    match t with
    | Component { name } -> name
    | Operation { operation_id; status } ->
      let status_infix =
        if success_response_for_operation
        then ""
        else [%string "%{status#Operation_response_status}_"]
      in
      [%string "response_%{status_infix}%{operation_id}"]
  ;;

  include Comparable.Make (struct
      type nonrec t = t [@@deriving sexp, compare]
    end)
end

module Type = struct
  type t =
    { name : Name.t
    ; structure : Type_structure.t
    }
  [@@deriving fields ~getters ~setters ~iterators:create, sexp]

  let create ~name ~structure () =
    Fields.create ~name:(Name.of_raw_string name) ~structure
  ;;
end

module Type_space = struct
  type t =
    { next_id : Type_id.t
    ; id_to_type : Type.t Type_id.Map.t
    ; ref_to_id : Type_id.t String.Map.t
    ; parameter_to_id : Type_id.t Parameter_origin.Map.t
    ; response_to_id : Type_id.t Response_origin.Map.t
    }
  [@@deriving sexp]

  let type_of_id ~type_id t = Map.find t.id_to_type type_id
  let ref_of_id ~ref_ t = Map.find t.ref_to_id ref_
  let to_map t = t.id_to_type

  let add_type ~type_ t =
    let next_id = Type_id.next t.next_id in
    let id_to_type = Map.add_exn ~key:t.next_id ~data:type_ t.id_to_type in
    t.next_id, { t with next_id; id_to_type }
  ;;

  let add_ref ~type_id ~ref_ t =
    let next_id = Type_id.next t.next_id in
    let id_to_type = t.id_to_type in
    let ref_to_id = Map.add_exn ~key:ref_ ~data:type_id t.ref_to_id in
    { t with next_id; id_to_type; ref_to_id }
  ;;

  let empty =
    { next_id = Type_id.init
    ; id_to_type = Type_id.Map.empty
    ; ref_to_id = String.Map.empty
    ; parameter_to_id = Parameter_origin.Map.empty
    ; response_to_id = Response_origin.Map.empty
    }
  ;;

  let is_just_jsonaf t type_id =
    match type_of_id ~type_id t with
    | Some { structure = Existing_type { name; _ }; _ } ->
      Type_description.Identifier.equal name Type_description.Presets.jsonaf.name
    | _ -> false
  ;;

  let map_of_options_to_map_option map =
    if Map.for_all map ~f:Option.is_some
    then Some (Map.map map ~f:(fun map -> Option.value_exn map))
    else None
  ;;

  let inspect_variant_discriminator
    ~(members : Schema.t Or_reference.t Type_id.Map.t)
    ~discriminator:{ Discriminator.property_name; mapping }
    =
    let%bind.Option member_references =
      Map.map members ~f:(function
        | Ref { ref_ } -> Some ref_
        | Value _ -> None)
      |> map_of_options_to_map_option
    in
    let%map.Option values =
      match mapping with
      | None ->
        (* The tag is the name of the component *)
        Map.map member_references ~f:Reference.last_segment
        |> map_of_options_to_map_option
      | Some mapping ->
        (* The mapping is from value to reference, but we want the reverse *)
        let%bind.Option mapping =
          Map.to_sequence mapping
          |> Sequence.map ~f:Tuple2.swap
          |> String.Map.of_sequence_or_error
          |> Or_error.ok
        in
        Map.map member_references ~f:(Map.find mapping) |> map_of_options_to_map_option
    in
    Variant_structures.Tagged_object { tag = property_name; values }
  ;;

  let inspect_variant_specialized t ~(members : Schema.t Or_reference.t Type_id.Map.t) =
    let check_unique_string_enum type_id =
      match%bind.Option type_of_id ~type_id t with
      | { structure = String_variant [ value ]; _ } -> Some (Name.to_raw_string value)
      | _ -> None
    in
    let unique_member_type =
      Map.keys members
      |> List.map ~f:(fun type_id ->
        match type_of_id ~type_id t with
        | Some { structure = Record _; _ } -> Some `Record
        | Some { structure = String_variant _; _ } -> Some `String_variant
        | _ -> None)
      |> Option.all
      |> Option.bind ~f:(fun result ->
        match
          List.remove_consecutive_duplicates
            result
            ~equal:[%equal: [ `Record | `String_variant ]]
        with
        | [ value ] -> Some value
        | _ -> None)
    in
    match%bind.Option unique_member_type with
    | `String_variant ->
      let%map.Option result =
        Map.mapi members ~f:(fun ~key ~data:_ -> check_unique_string_enum key)
        |> map_of_options_to_map_option
      in
      Variant_structures.String_enum result
    | `Record ->
      let collect_possible_object_tags type_id =
        match type_of_id ~type_id t with
        | Some { structure = Record { properties; _ }; _ } ->
          Map.filter_map properties ~f:(fun (type_id, optionality) ->
            match optionality with
            | `Required -> check_unique_string_enum type_id
            | `Optional -> None)
        | _ -> Name.Map.empty
      in
      let candidates =
        Map.keys members
        |> List.map ~f:(fun type_id ->
          collect_possible_object_tags type_id
          |> Map.map ~f:(fun key -> String.Map.singleton key type_id))
        |> List.reduce_exn
             ~f:
               (Map.merge_by_case
                  ~first:Drop (* Tag not shared *)
                  ~second:Drop (* Tag not shared *)
                  ~both:
                    (Filter_map
                       (fun ~key:_ mapping1 mapping2 ->
                         Option.try_with (fun () ->
                           (* This is the mapping from tag value to branch. If the mapping
                              is not disjoint, the tag is not uniquely identifying, and so
                              it's not viable *)
                           Map.merge_disjoint_exn mapping1 mapping2))))
      in
      let%map.Option tag, mapping = Map.min_elt candidates in
      let values =
        Map.to_sequence mapping
        |> Sequence.map ~f:Tuple2.swap
        |> Type_id.Map.of_sequence_exn
      in
      Variant_structures.Tagged_object { tag = Name.to_raw_string tag; values }
  ;;

  let inspect_variant
    t
    ~discriminator
    ~(members : Schema.t Or_reference.t Type_id.Map.t)
    ~components
    =
    match discriminator with
    | Some discriminator -> inspect_variant_discriminator ~discriminator ~members
    | None ->
      (match inspect_variant_specialized t ~members with
       | Some specialized -> Some specialized
       | None ->
         let%bind.Option () =
           (* Jsonaf object will catch all, making the transparent variant useless *)
           if Map.existsi members ~f:(fun ~key ~data:_ -> is_just_jsonaf t key)
           then None
           else Some ()
         in
         (* Last ditch effort, if we can find good names the transparent variants. *)
         let%map.Option names =
           Map.map members ~f:(fun reference ->
             let via_title =
               let%bind.Option schema = resolve_schema_ref ~components reference in
               schema.title
             in
             let via_reference =
               match reference with
               | Ref { ref_ } -> Reference.last_segment ref_
               | _ -> None
             in
             Option.first_some via_title via_reference)
           |> map_of_options_to_map_option
         in
         Variant_structures.Transparent_object { names })
  ;;

  let empty_schema = Schema.t_of_jsonaf (`Object [])

  let attempt_all_of_merge schemas ~components =
    let dedup_all_of schemas =
      (* Dedup the same refs. This can happen due to a merge. We sort to group identical
         Refs together; the [_] for Values is fine because we only deduplicate Refs. *)
      List.sort schemas ~compare:[%compare: _ Or_reference.t]
      |> List.remove_consecutive_duplicates ~equal:(fun a b ->
        match a, b with
        | Ref a, Ref b -> String.equal a.ref_ b.ref_
        | _ -> false)
    in
    let schemas = List.map schemas ~f:(resolve_schema_ref ~components) in
    List.reduce schemas ~f:(fun a b ->
      let%bind.Option a and b in
      (* Over approximating merge, most of these features we don't use anyway *)
      let unique_some ?merge a b =
        match a, b with
        | None, None -> Some None
        | Some a, Some b -> Option.bind merge ~f:(fun merge -> merge a b)
        | Some v, None | None, Some v -> Some (Some v)
      in
      let%map.Option type_ =
        unique_some a.type_ b.type_ ~merge:(fun a_type b_type ->
          if [%compare.equal: Schema.Type.t] a_type b_type
          then Some (Some a_type)
          else None)
      and enum =
        unique_some a.enum b.enum
        (* We can try to do a clever merge here by set intersection, but that's
           complicated especially around jsonaf *)
      and one_of = unique_some a.one_of b.one_of
      and any_of = unique_some a.any_of b.any_of
      and all_of =
        unique_some
          ~merge:(fun a b -> Some (Some (dedup_all_of (a @ b))))
          a.all_of
          b.all_of
      and items = unique_some a.items b.items
      and properties =
        unique_some a.properties b.properties ~merge:(fun properties_a properties_b ->
          Map.merge_by_case
            properties_a
            properties_b
            ~first:Keep
            ~second:Keep
            ~both:
              (Map
                 (fun ~key:_ a b ->
                   Or_reference.Value
                     { empty_schema with all_of = Some (dedup_all_of [ a; b ]) }))
          |> Some
          |> Some)
      and additional_properties =
        unique_some
          a.additional_properties
          b.additional_properties
          ~merge:(fun additional_properties_a additional_properties_b ->
            let result =
              match additional_properties_a, additional_properties_b with
              | _, `Not_allowed | `Not_allowed, _ -> `Not_allowed
              | `Allowed a, `Allowed b ->
                `Allowed
                  (Or_reference.Value
                     { empty_schema with all_of = Some (dedup_all_of [ a; b ]) })
            in
            Some (Some result))
      and discriminator = unique_some a.discriminator b.discriminator
      and format =
        unique_some a.format b.format ~merge:(fun a_format b_format ->
          if [%compare.equal: Schema.Format.t] a_format b_format
          then Some (Some a_format)
          else None)
      in
      { Schema.title = Option.first_some a.title b.title
      ; multiple_of = None
      ; maximum = None
      ; exclusive_maximum = None
      ; minimum = None
      ; exclusive_minimum = None
      ; max_length = None
      ; min_length = None
      ; unique_items = a.unique_items || b.unique_items
      ; max_properties = None
      ; min_properties = None
      ; pattern = None
      ; max_items = None
      ; min_items = None
      ; required =
          List.dedup_and_sort ~compare:[%compare: string] (a.required @ b.required)
      ; enum
      ; type_
      ; format
      ; one_of
      ; any_of
      ; all_of
      ; not_ = None (* We don't use it *)
      ; items
      ; properties
      ; additional_properties
      ; discriminator
      ; description = None
      ; default = None
      ; nullable = a.nullable && b.nullable
      ; read_only = a.read_only || b.read_only
      ; write_only = a.write_only || b.write_only
      ; xml = None
      ; external_docs = None
      ; example = None
      ; deprecated = a.deprecated || b.deprecated
      })
    |> Option.join
  ;;

  let rec add_schema ?name ~schema ~components t =
    let add_type ~t type_ =
      let new_id, new_t = add_type ~type_ t in
      new_id, new_t
    in
    let make_nullable ~t ?name type_id =
      match type_of_id ~type_id t with
      | Some { structure = Nullable _; _ } -> type_id, t
      | _ ->
        Type.create
          ~name:
            (match name with
             | None -> "nullable"
             | Some name -> [%string "%{name}_nullable"])
          ~structure:(Nullable type_id)
          ()
        |> add_type ~t
    in
    match schema with
    | Or_reference.Ref reference ->
      let ref_ = Reference.ref_ reference in
      ref_of_id ~ref_ t
      |> Option.map ~f:(fun type_id -> type_id, t)
      |> Option.value_or_thunk ~default:(fun () ->
        let resolved_reference =
          resolve_schema_ref ~components schema |> Option.value_exn
        in
        let schema = Or_reference.Value resolved_reference in
        let name = Reference.last_segment ref_ |> Option.value_exn in
        let type_id, new_t = add_schema ~name ~schema ~components t in
        type_id, add_ref ~type_id ~ref_ new_t)
    | Or_reference.Value schema ->
      let open Type_structure in
      let matching_preset =
        lazy
          (List.find Type_mapping.presets ~f:(fun preset ->
             Blang.eval preset.matches (function
               | Type type_ ->
                 Option.mem ~equal:[%compare.equal: Schema.Type.t] schema.type_ type_
               | Format format ->
                 Option.mem ~equal:[%compare.equal: Schema.Format.t] schema.format format)))
      in
      let add_existing t description =
        Type.create
          ~name:(Type_description.type_name description)
          ~structure:(Existing_type description)
          ()
        |> add_type ~t
      in
      let inner_type, t =
        (* We first check for enums before we check for presets because enums are more
           restricted strings, and we want enums to take precedence over generic strings. *)
        match schema, name, matching_preset with
        (* Tags - how tagged variants are represented in json-schema *)
        | ( { type_ = Some String; enum = Some (`String _ :: _ as string_variants); _ }
          , Some name
          , _ ) ->
          let is_nullable =
            List.exists string_variants ~f:(function
              | `Null -> true
              | _ -> false)
          in
          let string_variants =
            List.filter_map string_variants ~f:(function
              | `String s -> Some s
              | `Null -> None
              | other ->
                (* Preserving the sexp formatting so that we can look at the type of the
                   json *)
                let other_json_structure = Jsonaf.sexp_of_t other |> Sexp.to_string in
                failwith
                  [%string
                    "Encountered a non-string in an enum, which is not supported by this\n\
                    \ generator. Enum %{name} contained value \
                     \"%{other_json_structure}\", which is not a string."])
          in
          let variant_names = List.rev_map string_variants ~f:Name.of_raw_string in
          (match is_nullable with
           | false ->
             Type.create ~name ~structure:(String_variant variant_names) () |> add_type ~t
           | true ->
             let non_nullable, t =
               Type.create ~name ~structure:(String_variant variant_names) ()
               |> add_type ~t
             in
             make_nullable ~name ~t non_nullable)
        | _, _, (lazy (Some { description; _ })) -> add_existing t description
        | { type_ = Some Array; items = Some item_schema; _ }, _, _ ->
          let underlying_type, t = add_schema ?name ~schema:item_schema ~components t in
          let structure = List underlying_type in
          Type.create ~name:"list" ~structure () |> add_type ~t
        | ( { type_ = Some Object
            ; properties = Some properties
            ; required
            ; additional_properties
            ; _
            }
          , Some name
          , _ )
          when not (Map.is_empty properties) ->
          let properties, t =
            Map.fold
              properties
              ~init:(Name.Map.empty, t)
              ~f:(fun ~key ~data:schema (properties_map, t) ->
                let new_type, t =
                  add_schema ~name:(key ^ "_" ^ name) ~schema ~components t
                in
                let necessity =
                  if List.mem required key ~equal:String.equal
                  then `Required
                  else `Optional
                in
                ( Map.add_exn
                    properties_map
                    ~key:(Name.of_raw_string key)
                    ~data:(new_type, necessity)
                , t ))
          in
          let t, additional_properties =
            match additional_properties with
            | Some (`Allowed properties) ->
              let properties, t =
                add_schema
                  ~name:[%string "additional_properties_%{name}"]
                  ~schema:properties
                  ~components
                  t
              in
              t, `Explicit properties
            | None -> t, `Allowed
            | Some `Not_allowed -> t, `Not_allowed
          in
          let structure = Record { properties; additional_properties } in
          Type.create ~name ~structure () |> add_type ~t
        | ( { type_ = Some Object; additional_properties = Some (`Allowed schema); _ }
          , Some name
          , _ ) ->
          let content, t =
            add_schema ~name:(name ^ "_additional_property") ~schema ~components t
          in
          (* If we're just generating jsonaf map members, no point in creating a map. Some
             schemas use "additional_properties: true" to indicate this is an arbitrary
             scratchpad, and it's better to just let people derive jsonaf for that instead
             of forcing them to awkwardly go through the map.
          *)
          if is_just_jsonaf t content
          then add_existing t Type_description.Presets.jsonaf
          else Type.create ~name:"Map" ~structure:(Map content) () |> add_type ~t
        (* Passthrough all_of, one_of, any_of if they're single element lists. *)
        | { all_of = Some (schema :: []); _ }, _, _
        | { one_of = Some (schema :: []); _ }, _, _
        | { any_of = Some (schema :: []); _ }, _, _ ->
          add_schema ?name ~schema ~components t
        (* Variant Types *)
        | { one_of = Some (_ :: _ :: _ as members); discriminator; _ }, Some name, _ ->
          (* We resolve the contents of the schemas recursively first to understand their
             content. This is temporary due to naming:

             If all member schemas are references, then we don't need to worry about
             naming being bad (e.g. For a tagged reference seeing a bunch of [_{idx}] in
             the type name), and we can just reuse our temporary type space, otherwise, we
             readd types with better names. Unfortunately, the current code makes
             decisions about names way too early on instead of just specifying the
             relationship between types and picking names later, which leads to
             collisions, unnecessary escaping (e.g. picking [type_] for module names), and
             these names propagate, so if we pick a bad name, we can't just go in and fix
             it that easily, it's much easier to just recompute the space.
          *)
          let temporary_type_space, members_list =
            List.fold_mapi members ~init:t ~f:(fun idx t schema ->
              let new_type, t =
                add_schema ~name:[%string "%{name}_%{idx#Int}"] ~schema ~components t
              in
              t, (new_type, schema))
          in
          let maybe_rename name_mapping =
            let all_members_are_references =
              List.for_all members ~f:(function
                | Ref _ -> true
                | Value _ -> false)
            in
            if all_members_are_references
            then
              ( temporary_type_space
              , List.map members_list ~f:(fun (type_id, _) ->
                  type_id, Name.of_raw_string (Map.find_exn name_mapping type_id)) )
            else
              List.fold_map members_list ~init:t ~f:(fun t (old_type_id, schema) ->
                let variant_name = Map.find_exn name_mapping old_type_id in
                let new_type, t =
                  add_schema
                    ~name:[%string "%{name}_%{variant_name}"]
                    ~schema
                    ~components
                    t
                in
                t, (new_type, Name.of_raw_string variant_name))
          in
          let members = Type_id.Map.of_alist_exn members_list in
          (match
             inspect_variant temporary_type_space ~members ~components ~discriminator
           with
           | Some (String_enum mapping) ->
             let variant_strings =
               List.map members_list ~f:(fun (type_id, _) ->
                 Map.find_exn mapping type_id |> Name.of_raw_string)
             in
             Type.create ~name ~structure:(String_variant variant_strings) ()
             |> add_type ~t
           | Some (Tagged_object { tag; values }) ->
             let t, child_types = maybe_rename values in
             Type.create ~name ~structure:(Object_variant (child_types, tag)) ()
             |> add_type ~t
           | Some (Transparent_object { names }) ->
             let t, child_types = maybe_rename names in
             Type.create ~name ~structure:(Transparent_variant child_types) ()
             |> add_type ~t
           | None -> add_existing t Type_description.Presets.jsonaf)
        | { all_of = Some (_ :: _ :: _ as members); _ }, _, _ ->
          (* We merge the schema along with all the members, as the schema may bring
             additional constraints. *)
          (match
             attempt_all_of_merge
               ~components
               (Value { schema with all_of = None } :: members)
           with
           | Some schema -> add_schema ?name ~schema:(Value schema) ~components t
           | None -> add_existing t Type_description.Presets.jsonaf)
        | _ -> add_existing t Type_description.Presets.jsonaf
      in
      (match Schema.nullable schema with
       | false -> inner_type, t
       | true -> make_nullable inner_type ~t)
  ;;

  let add_schema_for_parameter t ~schema ~origin ~components =
    match Map.find t.parameter_to_id origin with
    | Some type_id -> type_id, t
    | None ->
      let name = Parameter_origin.raw_name origin in
      let type_id, t = add_schema t ~name ~schema ~components in
      let t =
        { t with
          parameter_to_id = Map.add_exn t.parameter_to_id ~key:origin ~data:type_id
        }
      in
      type_id, t
  ;;

  let add_schema_for_response
    t
    ~schema
    ~origin
    ~components
    ~success_response_for_operation
    =
    match Map.find t.response_to_id origin with
    | Some type_id -> type_id, t
    | None ->
      let name = Response_origin.raw_name origin ~success_response_for_operation in
      let type_id, t = add_schema t ~name ~schema ~components in
      let t =
        { t with response_to_id = Map.add_exn t.response_to_id ~key:origin ~data:type_id }
      in
      type_id, t
  ;;
end
