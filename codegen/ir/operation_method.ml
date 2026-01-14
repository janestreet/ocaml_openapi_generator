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
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

let create = Fields.create

(* Well formed specs have an operation ID, but many do not have one, so we turn the
   method + path into an operation ID and hope it doesn't collide. *)
let infer_operation_id ~path ~http_method =
  let replace_non_alphanumeric =
    String.map ~f:(fun c -> if Char.is_alphanum c then c else '_')
  in
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

let make_parameter_list ~path_parameters ~components ~operation_id ~type_space ~operation =
  (* External parameters are defined on the path itself (i.e. are valid for all HTTP
     methods done on the same path). They can be overridden by internal parameters, which
     are defined on the specific operation. *)
  let external_parameters = resolve_all_parameters ~components path_parameters in
  let internal_parameters =
    resolve_all_parameters ~components (Operation.parameters operation)
  in
  (* Here we resolve the Open_api parameters into an Operation_parameter.t list, which
     contains all information needed to later generate an OCaml function. *)
  Hashtbl.merge internal_parameters external_parameters ~f:(fun ~key:_ -> function
    | `Left l -> Some l
    | `Right r -> Some r
    | `Both (l, _r) -> Some l)
  |> Hashtbl.to_alist
  |> List.fold ~init:([], type_space) ~f:(fun (lst, type_space) (api_name, parameter) ->
    let parameter_data = Parameter.data parameter in
    [%log.debug "Processing parameter" (api_name : string) (operation_id : string)];
    (let%bind schema = parameter_data |> Parameter.Parameter_data.schema in
     let type_id, type_space =
       Type_space.add_schema ~name:api_name ~schema ~components type_space
     in
     let type_ = Operation_parameter.Operation_parameter_type.Type type_id in
     let description = Parameter.Parameter_data.description parameter_data in
     let create = Operation_parameter.create ~name:api_name ~description ~type_ in
     match parameter with
     | Path { parameter_data; _ } ->
       assert parameter_data.required;
       Some (create ~kind:Operation_parameter.Operation_parameter_kind.Path, type_space)
     | Query { parameter_data; _ } ->
       let required = parameter_data.required in
       Some
         ( create ~kind:(Operation_parameter.Operation_parameter_kind.Query required)
         , type_space )
     | Header _ ->
       [%log.error
         "Skipping parameter, headers are not supported."
           (api_name : string)
           (operation_id : string)];
       None
     | Cookie _ ->
       [%log.error
         "Skipping parameter, cookies are not supported."
           (api_name : string)
           (operation_id : string)];
       None)
    |> Option.map ~f:(fun (param, type_space) -> lst @ [ param ], type_space)
    |> Option.value ~default:(lst, type_space))
;;

let make_response_list ~operation ~components ~type_space =
  let response_list, type_space =
    Operation.responses operation
    |> Map.to_alist
    |> List.fold
         ~init:([], type_space)
         ~f:(fun (lst, type_space) (status, response_or_ref) ->
           (let open Operation_response in
            let open Operation_response_type in
            let%bind response = resolve_response_ref ~components response_or_ref in
            let status = Operation_response_status.of_string status in
            let%bind type_, type_space =
              let media_types = Response.content response in
              let non_binary_type =
                let%bind media_type =
                  Option.first_some
                    (Map.find media_types "application/ld+json")
                    (Map.find media_types "application/json")
                in
                let%map schema =
                  media_type
                  |> Media_type_or_server_sent_event.media_type
                  |> Media_type.schema
                in
                let type_id, type_space =
                  Type_space.add_schema ~schema ~components type_space
                in
                Resolved type_id, type_space
              in
              let binary_type =
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
                in
                let is_binary_type =
                  List.exists binary_content_types ~f:(fun content_type ->
                    Map.mem media_types content_type)
                in
                Option.some_if is_binary_type (Raw, type_space)
              in
              (* We arbitrarily choose the non-binary type first to preserve existing
                 behaviour. *)
              Option.first_some non_binary_type binary_type
            in
            Some (lst @ [ status, type_ ], type_space))
           |> Option.value ~default:(lst, type_space))
  in
  ( List.map response_list ~f:(fun (status_code, type_id) ->
      Operation_response.create ~status_code ~type_id ~description:None)
  , type_space )
;;

let of_operation ~path ~http_method ~path_parameters ~components ~type_space operation =
  let operation_id =
    Operation.operation_id operation
    |> Option.value ~default:(infer_operation_id ~path ~http_method)
  in
  (* Here we resolve the Open_api parameters into an Operation_parameter.t list, which
     contains all information needed to later generate an OCaml function. *)
  let parameters, type_space =
    make_parameter_list ~path_parameters ~components ~operation_id ~type_space ~operation
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
  let responses, type_space = make_response_list ~operation ~components ~type_space in
  let exists_default_response =
    Operation_response.get_default responses |> Option.is_some
  in
  let responses =
    let open Operation_response in
    if exists_default_response
    then responses
    else
      Operation_response.create
        ~status_code:Operation_response_status.Default
        ~type_id:Operation_response_type.None
        ~description:None
      :: responses
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
