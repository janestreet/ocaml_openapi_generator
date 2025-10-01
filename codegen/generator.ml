open! Core
open! Async
open Openapi_codegen_ir
open Openapi_codegen_ir.Typify
open Openapi_spec.Types
open Jingoo
open Jingoo.Jg_types
open Embedded_strings

let method_to_ocaml = function
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

let rec type_reference ~type_id ~type_space =
  (let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
   match Type.structure type_ with
   | Record _ | String_variant _ | Object_variant _ | Transparent_variant _ ->
     Some ((Type.name type_ |> Name.to_module_name) ^ ".t")
   | List type_id -> Some (type_reference ~type_id ~type_space ^ " list")
   | Set type_id -> Some (type_reference ~type_id ~type_space ^ " Set.t")
   | Optional type_id -> Some (type_reference ~type_id ~type_space ^ " option")
   | Nullable type_id -> Some (type_reference ~type_id ~type_space ^ " option")
   | Existing_type -> Some ((Type.name type_ |> Name.to_module_name) ^ ".t")
   | Specific_string _ -> Some "Jane_object_tag.t")
  |> Option.value ~default:"Jsonaf.t"
;;

let is_optional ~type_id ~type_space =
  (let%map.Option type_ = Type_space.type_of_id ~type_id type_space in
   match Type.structure type_ with
   | Optional _ -> true
   | _ -> false)
  |> Option.value ~default:false
;;

let env = { Jg_types.std_env with autoescape = false }

let type_definition
  ~type_id
  ~type_space
  ~raise_on_optional_null
  ~include_unknown_fallback_for_enums
  =
  let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
  let type_map_to_jingoo type_map =
    Map.mapi type_map ~f:(fun ~key:name ~data:type_id ->
      Tobj
        [ "name", Tstr (Name.to_variable_name name)
        ; "api_name", Tstr (Name.to_raw_string name)
        ; "optional", Tbool (is_optional ~type_id ~type_space)
        ; "type_reference", Tstr (type_reference ~type_id ~type_space)
        ])
    |> Map.to_alist
    |> List.map ~f:snd
  in
  let type_list_to_jingoo type_list =
    List.map type_list ~f:(fun type_id ->
      let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
      let%map.Option variant_name = Type.variant_name type_ in
      Tobj
        [ "name", Tstr (Name.to_variant variant_name)
        ; "ocaml", Tstr ("`String \"" ^ Name.to_raw_string variant_name ^ "\"")
        ])
    |> Option.all
  in
  let tagged_type_list_to_jingoo ~type_list ~tag =
    List.map type_list ~f:(fun (type_id, variant_name) ->
      let%map.Option type_ = Type_space.type_of_id ~type_id type_space in
      let variant_type_name = Type.name type_ in
      Tobj
        [ "name", Tstr (Name.to_variant variant_name)
        ; "api_name", Tstr (Name.to_raw_string variant_name)
        ; "module", Tstr (Name.to_module_name variant_type_name)
        ; "tag", Tstr tag
        ])
    |> Option.all
  in
  match Type.structure type_ with
  | Record type_map ->
    let models =
      [ "module_name", Tstr (Type.name type_ |> Name.to_module_name)
      ; "fields", Tlist (type_map_to_jingoo type_map)
      ; "raise_on_optional_null", Tbool raise_on_optional_null
      ]
    in
    Some (Jg_template.from_string ~env ~models record_definition_dot_jingoo)
  | String_variant type_list ->
    let models =
      [ "module_name", Tstr (Type.name type_ |> Name.to_module_name)
      ; "variants", Tlist (type_list_to_jingoo type_list |> Option.value ~default:[])
      ; "include_unknown", Tbool include_unknown_fallback_for_enums
      ]
    in
    Some (Jg_template.from_string ~env ~models variant_definition_dot_jingoo)
  | Object_variant (type_list, tag) ->
    let models =
      [ "module_name", Tstr (Type.name type_ |> Name.to_module_name)
      ; "variants", Tlist (tagged_type_list_to_jingoo ~type_list ~tag |> Option.value_exn)
      ; "tag", Tstr tag
      ]
    in
    Some (Jg_template.from_string ~env ~models variant_definition_object_dot_jingoo)
  | Transparent_variant type_list ->
    let models =
      [ "module_name", Tstr (Type.name type_ |> Name.to_module_name)
      ; ( "variants"
        , Tlist
            (tagged_type_list_to_jingoo
               ~type_list
               ~tag:"" (* tag isn't needed for transparent variants *)
             |> Option.value_exn) )
      ]
    in
    Some (Jg_template.from_string ~env ~models variant_definition_transparent_dot_jingoo)
  | Existing_type | List _ | Set _ | Specific_string _ | Optional _ | Nullable _ -> None
;;

let get_type_names ~type_space =
  let type_space_map = Type_space.to_map type_space in
  Map.to_alist type_space_map
  |> List.map ~f:(fun (type_id, type_) -> type_id, Type.name type_)
  |> List.map ~f:(fun (type_id, type_name) ->
    ( type_definition
        ~type_id
        ~type_space
        ~raise_on_optional_null:true
        ~include_unknown_fallback_for_enums:false
    , type_name ))
  |> List.filter_map ~f:(fun (type_definition, type_name) ->
    match type_definition with
    | None -> None
    | Some _ -> Some type_name)
  |> List.dedup_and_sort ~compare:Name.compare
;;

let make_type_mls ~type_space ~raise_on_optional_null ~include_unknown_fallback_for_enums =
  let type_space_map = Type_space.to_map type_space in
  Map.to_alist type_space_map
  |> List.map ~f:(fun (type_id, type_) -> type_id, Type.name type_)
  |> List.map ~f:(fun (type_id, type_name) ->
    ( type_definition
        ~type_id
        ~type_space
        ~raise_on_optional_null
        ~include_unknown_fallback_for_enums
    , type_name ))
  |> List.filter_map ~f:(fun (type_definition, type_name) ->
    match type_definition with
    | None -> None
    | Some type_definition -> Some (type_definition, type_name))
  |> List.map ~f:(fun (module_definition, type_name) ->
    let models = [ "module", Tstr module_definition ] in
    let definition_str =
      Jg_template.from_string ~env ~models type_definition_dot_ml_dot_jingoo
    in
    definition_str, type_name)
;;

let generate_to_query_parameter ~type_space operation_parameter =
  match Operation_parameter.type_ operation_parameter with
  | Raw_body -> None
  | Type type_id ->
    let nullable_to_string type_ =
      [%string
        {|Option.value_map ~default:"null" ~f:%{Type.name type_ |> Name.to_module_name}.to_string|}]
    in
    let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
    (match Type.structure type_ with
     | Existing_type ->
       Some
         [%string
           {|Openapi_runtime.Query_parameters.singleton %{Type.name type_ |> Name.to_module_name}.to_string|}]
     | Nullable type_id ->
       let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
       (match Type.structure type_ with
        | Existing_type ->
          Some
            [%string
              {|Openapi_runtime.Query_parameters.singleton (%{nullable_to_string type_})|}]
        | _ -> None)
     | List type_id ->
       let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
       let%map.Option to_string =
         match Type.structure type_ with
         | Existing_type ->
           Some [%string {|%{Type.name type_ |> Name.to_module_name}.to_string|}]
         | Nullable type_id ->
           let%bind.Option type_ = Type_space.type_of_id ~type_id type_space in
           (match Type.structure type_ with
            | Existing_type -> Some [%string {|(%{nullable_to_string type_})|}]
            | _ -> None)
         | _ -> None
       in
       [%string {|Openapi_runtime.Query_parameters.array %{to_string}|}]
     | Record _ -> Some [%string {|Openapi_runtime.Query_parameters.object_ ()|}]
     | _ -> None)
;;

let module_path_of_type type_ =
  String.split ~on:'.' type_
  |> List.rev
  |> List.tl
  |> Option.map ~f:List.rev
  |> Option.map ~f:(String.concat ~sep:".")
;;

let operation_model ~operation ~type_space =
  let operation_id = Operation_method.operation_id operation in
  let operation_path =
    Sexp.to_string
      (Openapi_runtime.Path_template.sexp_of_t (Operation_method.path operation))
  in
  let operation_parameters = Operation_method.parameters operation in
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
    operation_parameters
    |> List.filter_map ~f:(fun param ->
      match Operation_parameter.kind param with
      | Path -> Some param
      | _ -> None)
    |> List.map ~f:(fun param ->
      Tobj
        [ "name", Tstr (Operation_parameter.name param |> Name.to_variable_name)
        ; "api_name", Tstr (Operation_parameter.name param |> Name.to_raw_string)
        ])
  in
  let query_parameters =
    operation_parameters
    |> List.filter_map ~f:(fun param ->
      let%bind.Option is_required =
        match Operation_parameter.kind param with
        | Path | Header _ | Body _ -> None
        | Query is_required -> Some is_required
      in
      let%map.Option to_query_parameter = generate_to_query_parameter ~type_space param in
      Tobj
        [ "name", Tstr (Operation_parameter.name param |> Name.to_variable_name)
        ; "api_name", Tstr (Operation_parameter.name param |> Name.to_raw_string)
        ; "is_required", Tbool is_required
        ; "to_query_parameter", Tstr to_query_parameter
        ])
  in
  let body_module =
    let%bind.Option body in
    match Operation_parameter.type_ body with
    | Type type_id ->
      let reference = type_reference ~type_id ~type_space in
      let%map.Option base_module = module_path_of_type reference in
      if String.is_suffix reference ~suffix:"list"
      then [%string {|jsonaf_of_list %{base_module}|}]
      else base_module
    | Raw_body -> Some "Jsonaf_string"
  in
  let operation_response =
    Operation_response.get_default (Operation_method.responses operation)
    |> Option.value_exn
  in
  let response_module, is_raw_response =
    match Operation_response.type_id operation_response with
    | Resolved type_id ->
      type_reference ~type_id ~type_space |> module_path_of_type, false
    | Raw -> None, true
    | _ -> None, false
  in
  [ "operation_id", Tstr operation_id
  ; "operation_path", Tstr operation_path
  ; "has_body", Tbool (Option.is_some body)
  ; "body_module", Tstr (body_module |> Option.value ~default:"Jsonaf")
  ; "endpoint_method", Tstr (method_to_ocaml operation_method)
  ; "has_response", Tbool (Option.is_some response_module)
  ; "has_raw_response", Tbool is_raw_response
  ; "response_module", Tstr (response_module |> Option.value ~default:"")
  ; "path_parameters", Tlist path_parameters
  ; "query_parameters", Tlist query_parameters
  ]
;;

let make_operations_ml ~operation_lists =
  let models =
    [ ( "endpoints"
      , Tlist
          (List.map operation_lists ~f:(fun (key, _) ->
             Tstr (Name.to_truncated_name key |> String.capitalize))) )
    ]
  in
  Jg_template.from_string ~env ~models operations_dot_ml_dot_jingoo
;;

let make_types_ml ~types =
  let models =
    [ ( "types"
      , Tlist
          (List.map types ~f:(fun type_ ->
             Tstr (Name.to_truncated_name type_ |> String.capitalize))) )
    ]
  in
  Jg_template.from_string ~env ~models types_dot_ml_dot_jingoo
;;

let make_operation_definition_ml ~operation_list ~type_space =
  let models =
    [ ( "operations"
      , Tlist
          (List.map operation_list ~f:(fun operation ->
             Tobj (operation_model ~operation ~type_space))) )
    ]
  in
  Jg_template.from_string ~env ~models operation_definition_dot_jingoo
;;

let make_jbuild ~name ~spec_file ~paths =
  let models =
    [ "name", Tstr name
    ; "spec", Tstr spec_file
    ; "targets", paths |> Set.to_list |> String.concat ~sep:"\n" |> Tstr
    ]
  in
  Jg_template.from_string ~env ~models jbuild_dot_jingoo
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
        let operation_methods, type_space =
          List.fold
            operations
            ~init:([], type_space)
            ~f:(fun (lst, type_space) (http_method, op) ->
              Openapi_codegen_ir.Operation_method.of_operation
                ~path
                ~http_method
                ~path_parameters:[]
                ~components
                ~type_space
                op
              |> Option.map ~f:(fun (operation_method, type_space) ->
                lst @ [ operation_method ], type_space)
              |> Option.value ~default:(lst, type_space))
        in
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

let make_module_aliases_ml
  ~(operation_lists : (Name.t * Operation_method.t list) list)
  ~type_space
  =
  let type_file_names = get_type_names ~type_space |> List.map ~f:Name.to_module_name in
  let models =
    [ ( "modules"
      , Tlist
          (List.map operation_lists ~f:(fun (operation_path, _) ->
             Tobj
               [ "nice_name", Tstr (Name.to_module_name operation_path)
               ; ( "real_name"
                 , Tstr (Name.to_truncated_name operation_path |> String.capitalize) )
               ])
           @ List.map type_file_names ~f:(fun name ->
             Tobj [ "nice_name", Tstr name; "real_name", Tstr name ])) )
    ]
  in
  Jg_template.from_string ~env ~models module_aliases_dot_ml_dot_jingoo
;;

let non_colliding_operation_name names proposal =
  let rec non_colliding_operation_name try_ =
    let new_name =
      String.concat
        [ Name.to_raw_string proposal
        ; "_operations"
        ; (if try_ = 0 then "" else Int.to_string try_)
        ]
      |> Name.of_operation_path
    in
    if List.exists names ~f:(Name.filenames_equal new_name)
    then non_colliding_operation_name (try_ + 1)
    else new_name
  in
  if List.exists names ~f:(Name.filenames_equal proposal)
  then non_colliding_operation_name 0
  else proposal
;;

(* The operation list module names are purely used to write to a file. Let us just rename
   them to something unique if it collides with a type name. *)
let disambiguate_names (operation_lists, type_space) =
  let type_names =
    Type_space.to_map type_space |> Map.data |> List.map ~f:Typify.Type.name
  in
  let operation_names = List.map operation_lists ~f:fst in
  let _all_names, new_operation_lists =
    List.fold
      operation_lists
      ~init:(operation_names, [])
      ~f:(fun (operation_names, operation_lists) (path, operations) ->
        let operation_names_without_current =
          List.filter operation_names ~f:(Fn.non (Name.equal path))
        in
        let new_operation_name =
          non_colliding_operation_name (type_names @ operation_names_without_current) path
        in
        ( new_operation_name :: operation_names_without_current
        , (new_operation_name, operations) :: operation_lists ))
  in
  (* preserve original order *)
  List.rev new_operation_lists, type_space
;;

let make_files
  ~config
  ~api
  ~spec_file
  ~raise_on_optional_null
  ~include_unknown_fallback_for_enums
  =
  let components = Open_api.components api in
  let destination = Config.destination config in
  let files_written = Hash_set.create (module String) in
  let mkpath rel_path =
    Hash_set.add files_written rel_path;
    Filename.of_parts [ destination; String.lowercase rel_path ]
  in
  let type_space = Openapi_codegen_ir.Typify.Type_space.empty in
  let paths = Open_api.paths api in
  let operation_lists, type_space =
    make_operation_method_lists ~type_space ~paths ~components |> disambiguate_names
  in
  let%bind.Deferred () =
    Deferred.List.iter
      operation_lists
      ~how:(`Max_concurrent_jobs 16)
      ~f:(fun (key, operation_list) ->
        let filename = Name.to_truncated_name key ^ ".ml" in
        Writer.with_file (mkpath filename) ~f:(fun writer ->
          Writer.write writer (make_operation_definition_ml ~operation_list ~type_space);
          Deferred.return ()))
  in
  let%bind.Deferred () =
    Writer.with_file
      (mkpath (config.name ^ ".ml"))
      ~f:(fun writer ->
        Writer.write writer (make_module_aliases_ml ~operation_lists ~type_space);
        Deferred.return ())
  in
  let%bind.Deferred () =
    Writer.with_file (mkpath "operations.ml") ~f:(fun writer ->
      Writer.write writer (make_operations_ml ~operation_lists);
      Deferred.return ())
  in
  let%bind.Deferred () =
    let files_to_write =
      make_type_mls
        ~type_space
        ~raise_on_optional_null
        ~include_unknown_fallback_for_enums
    in
    Deferred.List.iter
      ~how:`Sequential
      files_to_write
      ~f:(fun (file_contents, filename) ->
        Writer.with_file
          (mkpath (Name.to_truncated_name filename ^ ".ml"))
          ~f:(fun writer ->
            Writer.write writer file_contents;
            Deferred.return ()))
  in
  let%bind.Deferred () =
    Writer.with_file (mkpath "types.ml") ~f:(fun writer ->
      Writer.write writer (make_types_ml ~types:(get_type_names ~type_space));
      Deferred.return ())
  in
  let%bind.Deferred () =
    Writer.with_file (mkpath "jane_object_tag.ml") ~f:(fun writer ->
      Writer.write writer jane_object_tag_dot_ml;
      Deferred.return ())
  in
  let%bind.Deferred () =
    Writer.with_file (mkpath "dune.corrected") ~f:(fun writer ->
      Writer.write
        writer
        (make_jbuild
           ~name:config.name
           ~spec_file
           ~paths:(Set.of_hash_set (module String) files_written));
      Deferred.return ())
  in
  Deferred.Or_error.return ()
;;
