open! Core
open! Async
open Typify
open Openapi_spec.Types

module Operation_parameter_type = struct
  type t =
    | Type of Type_id.t
    | Raw_body
  [@@deriving sexp]
end

module Operation_parameter_kind = struct
  type t =
    | Path
    | Query of bool
    | Header of bool
    | Body of Body_content_type.t
  [@@deriving sexp]
end

type t =
  { name : Name.t
  ; description : string option
  ; type_ : Operation_parameter_type.t
  ; kind : Operation_parameter_kind.t
  }
[@@deriving fields ~getters ~setters ~iterators:create, sexp]

let create ~name ~description ~type_ ~kind =
  Fields.create ~name:(Name.of_raw_string name) ~description ~type_ ~kind
;;

let of_body ?(name = "") ~components ~type_space body =
  let open Option.Let_syntax in
  if not (Request_body.content body |> Map.length = 1)
  then [%log.debug "more than one content type; results are best effort"];
  let%bind content, media_type = Request_body.content body |> Map.to_alist |> List.hd in
  let%bind schema = Media_type.schema media_type in
  let name = name ^ "_body" in
  let%bind content_type = Body_content_type.of_string content |> Or_error.ok in
  let type_, type_space =
    match content_type with
    | Octet_stream -> Operation_parameter_type.Raw_body, type_space
    | Json | Form_urlencoded ->
      let type_id, type_space =
        Type_space.add_schema ~name ~schema ~components type_space
      in
      Operation_parameter_type.Type type_id, type_space
  in
  let description = Request_body.description body in
  Some
    ( create
        ~name:"body"
        ~description
        ~type_
        ~kind:(Operation_parameter_kind.Body content_type)
    , type_space )
;;
