open! Core
open! Jsonaf.Export

exception Openapi_spec_violation of string

let combine_exn a b = Error.to_exn (Error.of_list [ Error.of_exn b; Error.of_exn a ])

module Jsonaf_string_map = struct
  include String.Map

  let t_of_jsonaf value_of_jsonaf = function
    | `Object lst ->
      List.map lst ~f:(fun (k, v) -> k, value_of_jsonaf v) |> String.Map.of_alist_exn
    | _ -> failwith "Jsonaf_string_map: t_of_jsonaf: must be JSON Object"
  ;;

  let jsonaf_of_t jsonaf_of_value values =
    `Object (Core.Map.to_alist values |> List.map ~f:(fun (k, v) -> k, jsonaf_of_value v))
  ;;
end

module Component_lookup_location = struct
  type t =
    | Parameters
    | Request_bodies
    | Responses
    | Schemas
end

module Reference = struct
  type 'a t = { ref_ : string [@key "$ref"] }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]

  let last_segment ref_ = ref_ |> String.split ~on:'/' |> List.last
end

module Or_reference = struct
  type 'a t =
    | Ref of 'a Reference.t
    | Value of 'a

  (* Preferring Ref over Value is arbitrary here. *)
  let t_of_jsonaf value_of_jsonaf ref_or_value =
    try Ref (Reference.t_of_jsonaf value_of_jsonaf ref_or_value) with
    | val_exn ->
      (try Value (value_of_jsonaf ref_or_value) with
       | ref_exn -> raise (combine_exn val_exn ref_exn))
  ;;

  let jsonaf_of_t jsonaf_of_value = function
    | Ref r -> Reference.jsonaf_of_t jsonaf_of_value r
    | Value v -> jsonaf_of_value v
  ;;

  let value_exn = function
    | Ref _ -> failwith "reference is not resolved"
    | Value v -> v
  ;;
end

module Contact = struct
  type t =
    { name : string option [@jsonaf.option]
    ; url : string option [@jsonaf.option]
    ; email : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module License = struct
  type t =
    { name : string
    ; url : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Info = struct
  type t =
    { title : string
    ; description : string option [@jsonaf.option]
    ; terms_of_service : string option [@jsonaf.option]
    ; contact : Contact.t option [@jsonaf.option]
    ; license : License.t option [@jsonaf.option]
    ; version : string
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Server_variable = struct
  type t =
    { enum : string list option [@jsonaf.option]
    ; default : string
    ; description : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Server = struct
  type t =
    { url : string
    ; description : string option [@jsonaf.option]
    ; variables : Server_variable.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module External_documentation = struct
  type t =
    { description : string option [@jsonaf.option]
    ; url : string
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Tag = struct
  type t =
    { name : string
    ; description : string option [@jsonaf.option]
    ; external_docs : External_documentation.t option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Security_requirement = struct
  type t = string list Jsonaf_string_map.t [@@deriving jsonaf]
end

module Example = struct
  type t =
    { summary : string option [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; value : Jsonaf.t option [@jsonaf.option]
    ; external_value : string option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Link = struct
  type t =
    { operation_ref : string option [@jsonaf.option]
    ; operation_id : string option [@jsonaf.option]
    ; parameters : Jsonaf.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    ; request_body : Jsonaf.t option [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; server : Server.t option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Discriminator = struct
  type t =
    { property_name : string
    ; mapping : string Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module XML = struct
  type t =
    { name : string option [@jsonaf.option]
    ; namespace : string option [@jsonaf.option]
    ; prefix : string option [@jsonaf.option]
    ; attribute : bool [@default false]
    ; wrapped : bool [@default false]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Schema = struct
  type t =
    { title : string option [@jsonaf.option]
    ; multiple_of : float option [@jsonaf.option]
    ; maximum : float option [@jsonaf.option]
    ; exclusive_maximum : bool option [@jsonaf.option]
    ; minimum : float option [@jsonaf.option]
    ; exclusive_minimum : bool option [@jsonaf.option]
    ; max_length : int option [@jsonaf.option]
    ; min_length : int option [@jsonaf.option]
    ; pattern : string option [@jsonaf.option]
    ; max_items : int option [@jsonaf.option]
    ; min_items : int option [@jsonaf.option]
    ; unique_items : bool [@default false]
    ; max_properties : int option [@jsonaf.option]
    ; min_properties : int option [@jsonaf.option]
    ; required : string list [@default []]
    ; enum : Jsonaf.t list option [@jsonaf.option]
    ; type_ : string option [@jsonaf.option] [@key "type"]
    ; all_of : t Or_reference.t list option [@jsonaf.option]
    ; one_of : t Or_reference.t list option [@jsonaf.option]
    ; any_of : t Or_reference.t list option [@jsonaf.option]
    ; not_ : t Or_reference.t option [@jsonaf.option] [@key "not"]
    ; items : t Or_reference.t option [@jsonaf.option]
    ; properties : t Or_reference.t Jsonaf_string_map.t option [@jsonaf.option]
    ; additional_properties : Jsonaf.t option [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; format : string option [@jsonaf.option]
    ; default : Jsonaf.t option [@jsonaf.option]
    ; nullable : bool [@default false]
    ; discriminator : Discriminator.t option [@jsonaf.option]
    ; read_only : bool [@default false]
    ; write_only : bool [@default false]
    ; xml : XML.t option [@jsonaf.option]
    ; external_docs : External_documentation.t option [@jsonaf.option]
    ; example : Jsonaf.t option [@jsonaf.option]
    ; deprecated : bool [@default false]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
  [@@jsonaf.allow_extra_fields]
end

module rec Header : sig
  type t =
    { description : string option
    ; required : bool
    ; deprecated : bool
    ; allow_empty_value : bool
    ; style : string option
    ; explode : bool option
    ; allow_reserved : bool
    ; schema : Schema.t Or_reference.t option
    ; example : Jsonaf.t option
    ; examples : Example.t Jsonaf_string_map.t
    ; content : Media_type.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf]
end = struct
  type t =
    { description : string option [@jsonaf.option]
    ; required : bool [@default false]
    ; deprecated : bool [@default false]
    ; allow_empty_value : bool [@default false]
    ; style : string option [@jsonaf.option]
    ; explode : bool option [@jsonaf.option]
    ; allow_reserved : bool [@default false]
    ; schema : Schema.t Or_reference.t option [@jsonaf.option]
    ; example : Jsonaf.t option [@jsonaf.option]
    ; examples : Example.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    ; content : Media_type.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

and Encoding : sig
  type t =
    { content_type : string option
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
    ; style : string option
    ; explode : bool option
    ; allow_reserved : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end = struct
  type t =
    { content_type : string option [@jsonaf.option]
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; style : string option [@jsonaf.option]
    ; explode : bool option [@jsonaf.option]
    ; allow_reserved : bool [@default false]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

and Media_type_or_server_sent_event : sig
  type t =
    | Server_sent_event of Media_type.t
    | Media_type of Media_type.t
  [@@deriving jsonaf]

  val media_type : t -> Media_type.t
end = struct
  type t =
    | Server_sent_event of Media_type.t
    | Media_type of Media_type.t

  let media_type = function
    | Server_sent_event media_type | Media_type media_type -> media_type
  ;;

  let t_of_jsonaf jsonaf =
    try Media_type ([%of_jsonaf: Media_type.t] jsonaf) with
    | _exn ->
      (match [%of_jsonaf: Media_type.t Jsonaf_string_map.t] jsonaf |> Map.to_alist with
       | [ ("x-server-sent-event", event) ] -> Server_sent_event event
       | _ ->
         raise_s [%message "Couldn't parse as media type or event" (jsonaf : Jsonaf.t)])
  ;;

  let jsonaf_of_t = function
    | Media_type media_type -> [%jsonaf_of: Media_type.t] media_type
    | Server_sent_event media_type ->
      Map.singleton (module String) "x-server-sent-event" media_type
      |> [%jsonaf_of: Media_type.t Jsonaf_string_map.t]
  ;;
end

and Media_type : sig
  type t =
    { schema : Schema.t Or_reference.t option
    ; example : Jsonaf.t option
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
    ; encoding : Encoding.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end = struct
  type t =
    { schema : Schema.t Or_reference.t option [@jsonaf.option]
    ; example : Jsonaf.t option [@jsonaf.option]
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; encoding : Encoding.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Parameter = struct
  module Parameter_data = struct
    type t =
      { name : string
      ; description : string option [@jsonaf.option]
      ; required : bool [@default false]
      ; deprecated : bool [@default false]
      ; example : Jsonaf.t option [@jsonaf.option]
      ; examples : Example.t Or_reference.t Jsonaf_string_map.t
           [@default Jsonaf_string_map.empty]
      ; explode : bool option [@jsonaf.option]
      ; schema : Schema.t Or_reference.t option [@jsonaf.option]
      ; content : Media_type.t Jsonaf_string_map.t option [@jsonaf.option]
      ; allow_reserved : bool option [@jsonaf.option]
      ; allow_empty_value : bool option [@jsonaf.option]
      }
    [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
    [@@jsonaf.allow_extra_fields]
  end

  module Query_style = struct
    type t =
      | Form
      | Space_delimited
      | Pipe_delimited
      | Deep_object
    [@@deriving jsonaf ~capitalize:"camelCase"]

    let jsonaf_of_t = function
      | Form -> `String "form"
      | Space_delimited -> `String "spaceDelimited"
      | Pipe_delimited -> `String "pipeDelimited"
      | Deep_object -> `String "deepObject"
    ;;

    let t_of_jsonaf = function
      | `String str ->
        (match str with
         | "form" -> Form
         | "spaceDelimited" -> Space_delimited
         | "pipeDelimited" -> Pipe_delimited
         | "deepObject" -> Deep_object
         | _ -> Form)
      | _ -> Form
    ;;
  end

  module Header_style = struct
    type t = Simple

    let jsonaf_of_t _ = `String "simple"
    let t_of_jsonaf _ = Simple
  end

  module Path_style = struct
    type t =
      | Matrix
      | Label
      | Simple

    let jsonaf_of_t = function
      | Matrix -> `String "matrix"
      | Label -> `String "label"
      | Simple -> `String "simple"
    ;;

    let t_of_jsonaf = function
      | `String str ->
        (match str with
         | "matrix" -> Matrix
         | "label" -> Label
         | "simple" -> Simple
         | _ -> Simple)
      | _ -> Simple
    ;;
  end

  module Cookie_style = struct
    type t = Form

    let jsonaf_of_t = function
      | Form -> `String "form"
    ;;

    let t_of_jsonaf _ = Form
  end

  type t =
    | Query of
        { parameter_data : Parameter_data.t
        ; style : Query_style.t
        }
    | Header of
        { parameter_data : Parameter_data.t
        ; style : Header_style.t
        }
    | Path of
        { parameter_data : Parameter_data.t
        ; style : Path_style.t
        }
    | Cookie of
        { parameter_data : Parameter_data.t
        ; style : Cookie_style.t
        }

  let get_style obj =
    List.find_map obj ~f:(fun (key, value) ->
      match key with
      | "style" -> Some value
      | _ -> None)
    |> Option.value ~default:`Null
  ;;

  let data = function
    | Query q -> q.parameter_data
    | Header h -> h.parameter_data
    | Path p -> p.parameter_data
    | Cookie c -> c.parameter_data
  ;;

  let t_of_jsonaf (json : Jsonaf.t) =
    match json with
    | `Object obj ->
      (match List.find_exn obj ~f:(fun (k, _v) -> String.equal k "in") with
       | _, `String "query" ->
         Query
           { parameter_data = Parameter_data.t_of_jsonaf json
           ; style = Query_style.t_of_jsonaf (get_style obj)
           }
       | _, `String "header" ->
         Header
           { parameter_data = Parameter_data.t_of_jsonaf json
           ; style = Header_style.t_of_jsonaf (get_style obj)
           }
       | _, `String "path" ->
         Path
           { parameter_data = Parameter_data.t_of_jsonaf json
           ; style = Path_style.t_of_jsonaf (get_style obj)
           }
       | _, `String "cookie" ->
         Cookie
           { parameter_data = Parameter_data.t_of_jsonaf json
           ; style = Cookie_style.t_of_jsonaf (get_style obj)
           }
       | _ -> raise (Openapi_spec_violation "field 'in' is required"))
    | _ -> raise (Openapi_spec_violation "field 'parameters' is not an object")
  ;;

  let jsonaf_of_t _t = failwith "Going back to JSON is unsupported."
end

module Request_body = struct
  type t =
    { description : string option [@jsonaf.option]
    ; content : Media_type.t Jsonaf_string_map.t
    ; required : bool [@default false]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
  [@@jsonaf.allow_extra_fields]
end

module Response = struct
  type t =
    { description : string
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; content : Media_type_or_server_sent_event.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; links : Link.t Or_reference.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Responses = struct
  type t = Response.t Or_reference.t Jsonaf_string_map.t [@@deriving jsonaf]
end

module rec Operation : sig
  type t =
    { tags : string list
    ; summary : string option
    ; description : string option
    ; external_docs : External_documentation.t option
    ; operation_id : string option
    ; parameters : Parameter.t Or_reference.t list
    ; request_body : Request_body.t Or_reference.t option
    ; responses : Responses.t
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
    ; deprecated : bool
    ; security : Security_requirement.t list option
    ; servers : Server.t list
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end = struct
  type t =
    { tags : string list [@default []]
    ; summary : string option [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; external_docs : External_documentation.t option [@jsonaf.option]
    ; operation_id : string option [@jsonaf.option]
    ; parameters : Parameter.t Or_reference.t list [@default []]
    ; request_body : Request_body.t Or_reference.t option [@jsonaf.option]
    ; responses : Responses.t
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; deprecated : bool [@jsonaf.default false]
    ; security : Security_requirement.t list option [@jsonaf.option]
    ; servers : Server.t list [@default []]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
  [@@jsonaf.allow_extra_fields]
end

and Path_item : sig
  type t =
    { ref_ : string option
    ; summary : string option
    ; description : string option
    ; get : Operation.t option
    ; put : Operation.t option
    ; post : Operation.t option
    ; delete : Operation.t option
    ; options : Operation.t option
    ; head : Operation.t option
    ; patch : Operation.t option
    ; trace : Operation.t option
    ; servers : Server.t list
    ; parameters : Parameter.t Or_reference.t list
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]

  val all_operations : t -> (Httpaf.Method.t * Operation.t) list
end = struct
  type t =
    { ref_ : string option [@key "$ref"] [@jsonaf.option]
    ; summary : string option [@jsonaf.option]
    ; description : string option [@jsonaf.option]
    ; get : Operation.t option [@jsonaf.option]
    ; put : Operation.t option [@jsonaf.option]
    ; post : Operation.t option [@jsonaf.option]
    ; delete : Operation.t option [@jsonaf.option]
    ; options : Operation.t option [@jsonaf.option]
    ; head : Operation.t option [@jsonaf.option]
    ; patch : Operation.t option [@jsonaf.option]
    ; trace : Operation.t option [@jsonaf.option]
    ; servers : Server.t list [@default []]
    ; parameters : Parameter.t Or_reference.t list [@default []]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
  [@@jsonaf.allow_extra_fields]

  let all_operations t =
    [ `GET, t.get
    ; `PUT, t.put
    ; `POST, t.post
    ; `DELETE, t.delete
    ; `OPTIONS, t.options
    ; `HEAD, t.head
    ; `Other "PATCH", t.patch
    ; `TRACE, t.trace
    ]
    |> List.map ~f:(fun (http_method, opt) ->
      match opt with
      | Some operation -> Some (http_method, operation)
      | None -> None)
    |> List.filter_opt
  ;;
end

and Callback : sig
  type t = Path_item.t [@@deriving jsonaf]
end = struct
  type t = Path_item.t [@@deriving jsonaf]
end

module Paths = struct
  type t = Path_item.t Jsonaf_string_map.t [@@deriving jsonaf]
end

module Oauth_flow = struct
  type t =
    { authorization_url : string option [@jsonaf.option]
    ; token_url : string option [@jsonaf.option]
    ; refresh_url : string option [@jsonaf.option]
    ; scopes : string Jsonaf_string_map.t
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Oauth_flows = struct
  type t =
    { implicit : Oauth_flow.t option [@jsonaf.option]
    ; password : Oauth_flow.t option [@jsonaf.option]
    ; client_credentials : Oauth_flow.t option [@jsonaf.option]
    ; authorization_code : Oauth_flow.t option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Security_scheme = struct
  type t =
    { type_ : string [@key "type"]
    ; description : string option [@jsonaf.option]
    ; name : string option [@jsonaf.option]
    ; in_ : string option [@key "in"] [@jsonaf.option]
    ; scheme : string option [@jsonaf.option]
    ; bearer_format : string option [@jsonaf.option]
    ; flows : Oauth_flows.t option [@jsonaf.option]
    ; open_id_connect_url : string option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Components = struct
  type t =
    { schemas : Schema.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; responses : Response.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; parameters : Parameter.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; request_bodies : Request_body.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; security_schemes : Security_scheme.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    ; links : Link.t Or_reference.t Jsonaf_string_map.t [@default Jsonaf_string_map.empty]
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end

module Open_api = struct
  type t =
    { openapi : string
    ; info : Info.t
    ; servers : Server.t list option [@jsonaf.option]
    ; paths : Paths.t
    ; components : Components.t [@default Components.t_of_jsonaf (Jsonaf.of_string "{}")]
    ; security : Security_requirement.t list option [@jsonaf.option]
    ; tags : Tag.t list option [@jsonaf.option]
    ; external_docs : External_documentation.t option [@jsonaf.option]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create]
end
