open! Core
open! Async
open Openapi_spec.Types
open Openapi_spec.Utils

module Type_id = struct
  module T = struct
    type t = int [@@deriving compare, sexp]

    let init = 0
    let next t = t + 1
  end

  include T
  module Map = Map.Make (T)
end

module Variant_structures = struct
  type t =
    | String_enum
    | Tagged_object of string
    | Transparent_object
  [@@deriving sexp]
end

module Type_structure = struct
  type t =
    | String_variant of Type_id.t list
    | Object_variant of (Type_id.t * Name.t) list * string
    | Transparent_variant of (Type_id.t * Name.t) list
    | Record of Type_id.t Name.Map.t
    | Specific_string of string
    | List of Type_id.t
    | Set of Type_id.t
    | Optional of Type_id.t
    | Nullable of Type_id.t
    | Existing_type
  [@@deriving sexp]
end

module Type = struct
  type t =
    { name : Name.t
    ; structure : Type_structure.t
    ; variant_name : Name.t option
    }
  [@@deriving fields ~getters ~setters ~iterators:create, sexp]

  let create ?namespace ?variant_name ~name ~structure () =
    Fields.create
      ~name:(Name.of_raw_string ?namespace name)
      ~variant_name:(Option.map variant_name ~f:Name.of_raw_string)
      ~structure
  ;;
end

module Type_space = struct
  type t =
    { next_id : Type_id.t
    ; id_to_type : Type.t Type_id.Map.t
    ; ref_to_id : Type_id.t String.Map.t
    }
  [@@deriving sexp]

  let type_of_id ~type_id t = Map.find t.id_to_type type_id
  let ref_of_id ~ref_ t = Map.find t.ref_to_id ref_
  let to_map t = t.id_to_type

  let add_type ~type_ t =
    let next_id = Type_id.next t.next_id in
    let id_to_type = Map.add_exn ~key:t.next_id ~data:type_ t.id_to_type in
    let ref_to_id = t.ref_to_id in
    t.next_id, { next_id; id_to_type; ref_to_id }
  ;;

  let add_ref ~type_id ~ref_ t =
    let next_id = Type_id.next t.next_id in
    let id_to_type = t.id_to_type in
    let ref_to_id = Map.add_exn ~key:ref_ ~data:type_id t.ref_to_id in
    { next_id; id_to_type; ref_to_id }
  ;;

  let empty =
    { next_id = Type_id.init
    ; id_to_type = Type_id.Map.empty
    ; ref_to_id = String.Map.empty
    }
  ;;

  let inspect_variant ~schemas ~components =
    let try_string_enum (schema : Schema.t) =
      match schema with
      | { type_ = Some "string"; enum = Some (`String variant_name :: []); _ } ->
        Some variant_name
      | _ -> None
    in
    (* returns the set of object keys within an object schema that point to a specific
       string (i.e. a string type restricted to one value) *)
    let get_possible_tag_names (schema : Schema.t) =
      match schema with
      | { type_ = Some "object"; properties = Some properties; _ } ->
        Map.mapi properties ~f:(fun ~key:_ ~data:ref_ ->
          let%bind.Option schema = resolve_schema_ref ~components ref_ in
          try_string_enum schema)
        |> Map.to_alist
        |> List.filter ~f:(fun (_, opt) -> Option.is_some opt)
        |> List.map ~f:fst
        |> String.Set.of_list
      | _ -> String.Set.empty
    in
    let try_transparently_tagged_object (schema : Schema.t) =
      match schema with
      | { title = Some title; _ } -> Some title
      | _ -> None
    in
    let%bind.Option schemas =
      List.map schemas ~f:(resolve_schema_ref ~components) |> Option.all
    in
    let schema_kinds = List.map schemas ~f:Schema.type_ |> Option.all in
    let schema_kind =
      Option.bind schema_kinds ~f:(fun kind -> List.all_equal kind ~equal:String.equal)
    in
    match schema_kind with
    | Some "string" ->
      List.map schemas ~f:try_string_enum
      |> Option.all
      |> Option.map ~f:(fun _ -> Variant_structures.String_enum)
    | Some "object" ->
      (* If this is a tagged variant, the object itself should have a field (e.x. called
         "tag") that contains some string that identifies which variant it is.

         [get_possible_tag_names] returns a set of these such strings, and we intersect
         that set across all the objects in the variant to infer that this is a tagged
         object.
      *)
      List.map schemas ~f:get_possible_tag_names
      |> List.reduce ~f:Set.inter
      |> Option.bind ~f:(fun set -> Set.find set ~f:(fun _ -> true))
      |> Option.map ~f:(fun tag -> Variant_structures.Tagged_object tag)
    | _ ->
      List.map schemas ~f:try_transparently_tagged_object
      |> Option.all
      |> Option.map ~f:(fun _ -> Variant_structures.Transparent_object)
  ;;

  (** Always safe to call on values that return Tagged_object from [inspect_variant]. *)
  let get_tagged_variant_exn ~tag ~schema ~components =
    let parameter_map =
      resolve_schema_ref ~components schema
      |> Option.value_exn
      |> Schema.properties
      |> Option.value_exn
    in
    let schema = Map.find_exn parameter_map tag in
    resolve_schema_ref ~components schema
    |> Option.value_exn
    |> Schema.enum
    |> Option.value_exn
    |> List.hd_exn
    |> Jsonaf.string_exn
  ;;

  let rec add_schema ?name ?(singleton_enum_in_variant = false) ~schema ~components t =
    let add_type ~t type_ =
      let new_id, new_t = add_type ~type_ t in
      new_id, new_t
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
      let jane_with_jsonaf_namespace = "Openapi_runtime.Jane_with_json" in
      let inner_type, t =
        match schema, name with
        (* Integer types *)
        | { type_ = Some "integer"; format = Some "int64"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_int64"
            ~structure:Existing_type
            ()
          |> add_type ~t
        | { type_ = Some "integer"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_int"
            ~structure:Existing_type
            ()
          |> add_type ~t
        | { type_ = Some "number"; format = Some "double"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_float"
            ~structure:Existing_type
            ()
          |> add_type ~t
        (* Boolean *)
        | { type_ = Some "boolean"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_bool"
            ~structure:Existing_type
            ()
          |> add_type ~t
        (* IP Addresses *)
        | { type_ = Some "string"; format = Some "ip"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_ip"
            ~structure:Existing_type
            ()
          |> add_type ~t
        | { type_ = Some "string"; format = Some "ipv4"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_ipv4"
            ~structure:Existing_type
            ()
          |> add_type ~t
        | { type_ = Some "string"; format = Some "ipv6"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_ipv6"
            ~structure:Existing_type
            ()
          |> add_type ~t
        (* Timestamps *)
        | { type_ = Some "string"; format = Some "date-time"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_time"
            ~structure:Existing_type
            ()
          |> add_type ~t
        (* UUIDs *)
        | { type_ = Some "string"; format = Some "uuid"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_uuid"
            ~structure:Existing_type
            ()
          |> add_type ~t
        (* Tags - how tagged variants are represented in json-schema *)
        | ( { type_ = Some "string"; enum = Some (`String _ :: _ as string_variants); _ }
          , Some name )
          when not singleton_enum_in_variant ->
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
          let variant_types =
            List.map string_variants ~f:(fun variant_name ->
              Type.create
                ~name:(name ^ "_" ^ variant_name)
                ~structure:(Specific_string name)
                ~variant_name
                ())
          in
          let type_ids, t =
            List.fold variant_types ~init:([], t) ~f:(fun (type_ids, accum_t) type_ ->
              let type_id, accum_t = add_type type_ ~t:accum_t in
              type_id :: type_ids, accum_t)
          in
          (match is_nullable with
           | false ->
             Type.create ~name ~structure:(String_variant type_ids) () |> add_type ~t
           | true ->
             let non_nullable, t =
               Type.create
                 ~name:(name ^ "_non_nullable")
                 ~structure:(String_variant type_ids)
                 ()
               |> add_type ~t
             in
             Type.create ~name ~structure:(Nullable non_nullable) () |> add_type ~t)
        | ( { type_ = Some "string"; enum = Some (`String variant_name :: []); _ }
          , Some name )
          when singleton_enum_in_variant ->
          Type.create
            ~name:(name ^ "_" ^ variant_name)
            ~structure:(Specific_string variant_name)
            ~variant_name
            ()
          |> add_type ~t
        | { type_ = Some "string"; enum = Some (`String _ :: _); _ }, Some name
          when singleton_enum_in_variant ->
          failwith
            [%string
              "singleton_enum_in_variant passed for %{name}, but multiple elements found \
               in enum list."]
        (* Generic String *)
        | { type_ = Some "string"; _ }, _ ->
          Type.create
            ~namespace:jane_with_jsonaf_namespace
            ~name:"Jsonaf_string"
            ~structure:Existing_type
            ()
          |> add_type ~t
        | { type_ = Some "array"; items = Some item_schema; _ }, _ ->
          let underlying_type, t = add_schema ?name ~schema:item_schema ~components t in
          let structure = List underlying_type in
          Type.create ~name:"list" ~structure () |> add_type ~t
        | { type_ = Some "object"; properties = Some properties; required; _ }, Some name
          ->
          let properties_map, t =
            Map.fold
              properties
              ~init:(Name.Map.empty, t)
              ~f:(fun ~key ~data:schema (properties_map, t) ->
                let new_type, t =
                  add_schema ~name:(key ^ "_" ^ name) ~schema ~components t
                in
                let new_type, t =
                  if List.mem required key ~equal:String.equal
                  then new_type, t
                  else
                    Type.create ~name:"option" ~structure:(Optional new_type) ()
                    |> add_type ~t
                in
                Map.add_exn properties_map ~key:(Name.of_raw_string key) ~data:new_type, t)
          in
          if Map.is_empty properties_map
          then
            Type.create ~name:"Jsonaf" ~structure:Type_structure.Existing_type ()
            |> add_type ~t
          else (
            let structure = Record properties_map in
            Type.create ~name ~structure () |> add_type ~t)
        | ( { type_ = Some "object"; additional_properties = Some (`Object properties); _ }
          , Some name ) ->
          let schema = Or_reference.t_of_jsonaf Schema.t_of_jsonaf (`Object properties) in
          add_schema ~name ~schema ~components t
        (* Passthrough all_of, any_of if they're single element lists. *)
        (* Don't passthrough one_of because it might be a single element variant. *)
        | { all_of = Some (schema :: []); _ }, _ -> add_schema ?name ~schema ~components t
        | { any_of = Some (schema :: []); _ }, _ -> add_schema ?name ~schema ~components t
        (* Variant Types *)
        | { one_of = Some schemas; _ }, Some name ->
          (match inspect_variant ~components ~schemas with
           | Some String_enum ->
             let child_types, t =
               List.foldi schemas ~init:([], t) ~f:(fun idx (lst, t) schema ->
                 let new_type, t =
                   add_schema
                     ~singleton_enum_in_variant:true
                     ~name:(name ^ "_" ^ Int.to_string idx)
                     ~schema
                     ~components
                     t
                 in
                 lst @ [ new_type ], t)
             in
             Type.create ~name ~structure:(String_variant child_types) () |> add_type ~t
           | Some (Tagged_object tag) ->
             let child_types, t =
               List.fold schemas ~init:([], t) ~f:(fun (lst, t) schema ->
                 let variant_name = get_tagged_variant_exn ~schema ~tag ~components in
                 let new_type, t =
                   add_schema ~name:(name ^ "_" ^ variant_name) ~schema ~components t
                 in
                 lst @ [ new_type, Name.of_raw_string variant_name ], t)
             in
             Type.create ~name ~structure:(Object_variant (child_types, tag)) ()
             |> add_type ~t
           | Some Transparent_object ->
             let child_types, t =
               List.fold schemas ~init:([], t) ~f:(fun (lst, t) schema ->
                 let variant_name =
                   resolve_schema_ref ~components schema
                   |> Option.value_exn
                   |> Schema.title
                   |> Option.value_exn
                 in
                 let new_type, t =
                   add_schema ~name:(name ^ "_" ^ variant_name) ~schema ~components t
                 in
                 lst @ [ new_type, Name.of_raw_string variant_name ], t)
             in
             Type.create ~name ~structure:(Transparent_variant child_types) ()
             |> add_type ~t
           | None -> Type.create ~name:"Jsonaf" ~structure:Existing_type () |> add_type ~t)
        | _ -> Type.create ~name:"Jsonaf" ~structure:Existing_type () |> add_type ~t
      in
      (match Schema.nullable schema with
       | false -> inner_type, t
       | true ->
         Type.create ~name:"nullable" ~structure:(Nullable inner_type) () |> add_type ~t)
  ;;
end
