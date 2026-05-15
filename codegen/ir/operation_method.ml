open! Core
open! Async
open Typify
open Openapi_runtime
open Openapi_spec.Types
open Openapi_spec.Utils
open Option.Let_syntax

type t =
  { operation_id : string
  ; tags : string list
  ; http_method : Httpaf_sexpable.Method.t
  ; path : Path_template.t
  ; summary : string option
  ; description : string option
  ; parameters : Operation_parameter.t list
  ; responses : Operation_response.t list
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp_of]

let create = Fields.create

let replace_non_alphanumeric s =
  String.map s ~f:(fun c -> if Char.is_alphanum c then c else '_')
;;

(* Well formed specs have an operation ID, but many do not have one, so we turn the
   method + path into an operation ID and hope it doesn't collide. *)
let infer_operation_id ~path ~http_method =
  let safe_str =
    String.lowercase
      (Httpaf_sexpable.Method.to_string http_method ^ replace_non_alphanumeric path)
  in
  let a =
    String.to_list safe_str
    |> List.mapi ~f:(fun ind c -> ind, c)
    |> List.filter_map ~f:(fun (ind, ch) ->
      if ind > 0 && Char.equal ch '_' && Char.equal safe_str.[ind - 1] '_'
      then None
      else Some ch)
    |> String.of_list
  in
  a
;;

module Resolved_paramater = struct
  type t =
    { parameter : Parameter.t
    ; origin : Parameter_origin.t
    }

  let resolve parameter_or_ref ~components ~origin =
    let parameter =
      resolve_parameter_ref ~components parameter_or_ref |> Option.value_exn
    in
    let component_name =
      match parameter_or_ref with
      | Value _ -> None
      | Ref { ref_ } -> Reference.last_segment ref_
    in
    let origin : Parameter_origin.t =
      match component_name, origin with
      | Some name, _ -> Component { name }
      | None, `Operation operation_id ->
        Parameter_origin.Operation { operation_id; parameter_name = parameter.name }
      | None, `Path path ->
        Parameter_origin.Path { path; parameter_name = parameter.name }
    in
    { parameter; origin }
  ;;

  let to_operation_parameter type_space { parameter; origin } ~components ~operation_id =
    [%log.debug
      "Processing parameter"
        (parameter.name : string)
        (operation_id : string)
        (origin : Parameter_origin.t)];
    let param_and_type_space =
      let%bind schema =
        match Parameter.schema parameter with
        | Directly_defined schema -> Some schema
        | Via_media_type { media_type = _; content } -> content.schema
      in
      let type_id, type_space =
        Type_space.add_schema_for_parameter ~schema ~components ~origin type_space
      in
      let type_ = Operation_parameter.Operation_parameter_type.Type type_id in
      let description = Parameter.description parameter in
      let explode = Parameter.explode parameter in
      let create =
        Operation_parameter.create ~name:parameter.name ~description ~type_ ~explode
      in
      match parameter.in_ with
      | Path _ ->
        assert parameter.required;
        Some (create ~kind:Path, type_space)
      | Query { style; _ } ->
        let required = parameter.required in
        Some (create ~kind:(Query { required; style }), type_space)
      | Header _ ->
        [%log.error
          "Skipping parameter, headers are not supported."
            (parameter.name : string)
            (operation_id : string)];
        None
      | Cookie _ ->
        [%log.error
          "Skipping parameter, cookies are not supported."
            (parameter.name : string)
            (operation_id : string)];
        None
    in
    match param_and_type_space with
    | None -> type_space, None
    | Some (param, type_space) -> type_space, Some param
  ;;
end

let make_parameter_list
  ~path
  ~path_parameters
  ~components
  ~operation_id
  ~type_space
  ~operation
  =
  let resolve_all_parameters ~components parameters ~origin =
    List.map parameters ~f:(Resolved_paramater.resolve ~components ~origin)
    |> String.Map.of_list_with_key_exn ~get_key:(fun { parameter; _ } -> parameter.name)
  in
  let parameters_from_path =
    resolve_all_parameters ~components path_parameters ~origin:(`Path path)
  in
  let parameters_from_operation =
    resolve_all_parameters
      ~components
      (Operation.parameters operation)
      ~origin:(`Operation operation_id)
  in
  let type_space, parameters =
    (* Here we resolve the Open_api parameters into an Operation_parameter.t list, which
       contains all information needed to later generate an OCaml function. *)
    Map.merge_by_case
      parameters_from_operation
      parameters_from_path
      ~both:Keep_first
      ~first:Keep
      ~second:Keep
    |> Map.data
    |> List.fold_map
         ~init:type_space
         ~f:(Resolved_paramater.to_operation_parameter ~components ~operation_id)
  in
  type_space, List.filter_opt parameters
;;

let json_content_types = [ "application/ld+json"; "application/json" ]

let binary_content_types =
  [ "application/pdf"
  ; "application/octet-stream"
  ; "application/zip"
  ; "image/png"
  ; "image/jpeg"
  ; "image/gif"
  ; "application/vnd.ms-excel"
  ; "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  ; "application/msword"
  ; "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  ; "text/csv" (* CSV files should also be returned as bytes *)
  ; "text/plain" (* Plain text files might also need byte handling *)
  ]
;;

module Resolved_response = struct
  type t =
    { response : Response.t
    ; status : Operation_response_status.t
    ; origin : Response_origin.t
    ; content : [ `Raw | `Json of Schema.t Or_reference.t ]
    }

  let resolve response_or_ref ~components ~operation_id ~status =
    let%bind.Option response = resolve_response_ref ~components response_or_ref in
    let origin : Response_origin.t =
      match response_or_ref with
      | Ref { ref_ } -> Component { name = ref_ }
      | Value _ -> Operation { status; operation_id }
    in
    let media_types = Response.content response in
    let json_schema =
      List.find_map json_content_types ~f:(fun content_type ->
        let%bind media_type = Map.find media_types content_type in
        Media_type_or_server_sent_event.media_type media_type |> Media_type.schema)
    in
    let%map.Option content =
      match json_schema with
      | None ->
        let is_binary_type = List.exists binary_content_types ~f:(Map.mem media_types) in
        if is_binary_type then Some `Raw else None
      | Some schema -> Some (`Json schema)
    in
    { response; status; origin; content }
  ;;

  let to_operation_response
    type_space
    { response; status; origin; content }
    ~components
    ~success_response_for_operation
    =
    let success_response_for_operation =
      Option.mem
        ~equal:[%equal: Operation_response_status.t]
        success_response_for_operation
        status
    in
    let type_space, (type_id : Operation_response.Operation_response_type.t) =
      match content with
      | `Json schema ->
        let type_id, type_space =
          Type_space.add_schema_for_response
            type_space
            ~schema
            ~origin
            ~components
            ~success_response_for_operation
        in
        type_space, Resolved type_id
      | `Raw -> type_space, Raw
    in
    let response =
      Operation_response.create
        ~status_code:status
        ~type_id
        ~description:
          (Option.some_if
             (not (String.is_empty response.description))
             response.description)
        ~success_response_for_operation
    in
    type_space, response
  ;;
end

let make_response_list ~operation ~operation_id ~components ~type_space =
  let responses =
    Operation.responses operation
    |> Map.map_keys_exn
         (module Operation_response_status)
         ~f:Operation_response_status.of_string
    |> Map.filter_mapi ~f:(fun ~key:status ~data:response_or_ref ->
      Resolved_response.resolve response_or_ref ~components ~operation_id ~status)
  in
  let success_response_for_operation =
    Map.keys responses |> Operation_response_status.pick_primary_success_response
  in
  let type_space, responses =
    Map.data responses
    |> List.fold_map
         ~init:type_space
         ~f:
           (Resolved_response.to_operation_response
              ~components
              ~success_response_for_operation)
  in
  let responses =
    match success_response_for_operation with
    | None ->
      Operation_response.create
        ~status_code:Default
        ~type_id:None
        ~description:None
        ~success_response_for_operation:true
      :: responses
    | Some _ -> responses
  in
  type_space, responses
;;

let of_operation ~path ~http_method ~path_parameters ~components ~type_space operation =
  let operation_id =
    Operation.operation_id operation
    |> Option.value ~default:(infer_operation_id ~path ~http_method)
  in
  (* Here we resolve the Open_api parameters into an Operation_parameter.t list, which
     contains all information needed to later generate an OCaml function. *)
  let type_space, parameters =
    make_parameter_list
      ~path
      ~path_parameters
      ~components
      ~operation_id
      ~type_space
      ~operation
  in
  (* Resolve the body parameter, which is special. *)
  let body_parameter_and_type_space =
    let%bind body = Operation.request_body operation in
    let%bind body = resolve_request_body_ref ~components body in
    Operation_parameter.of_body ~name:operation_id ~components ~type_space body
  in
  let type_space =
    Option.map body_parameter_and_type_space ~f:snd |> Option.value ~default:type_space
  in
  let body_parameter = Option.map body_parameter_and_type_space ~f:fst in
  let parameters = parameters @ ([ body_parameter ] |> List.filter_opt) in
  let%bind path = Path_template.of_template_string path in
  let type_space, responses =
    make_response_list ~operation_id ~operation ~components ~type_space
  in
  Some
    ( create
        ~operation_id
        ~tags:(Operation.tags operation)
        ~http_method
        ~path
        ~summary:(Operation.summary operation)
        ~description:(Operation.description operation)
        ~parameters
        ~responses
    , type_space )
;;
