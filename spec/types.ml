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
  [@@deriving sexp_of]
end

module Reference = struct
  type 'a t = { ref_ : string [@key "$ref"] }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of, compare]

  let last_segment ref_ = ref_ |> String.split ~on:'/' |> List.last
end

module Or_reference = struct
  type 'a t =
    | Ref of 'a Reference.t
    | Value of 'a
  [@@deriving sexp_of, compare]

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
    { name : string option [@jsonaf.option] [@sexp.option]
    ; url : string option [@jsonaf.option] [@sexp.option]
    ; email : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module License = struct
  type t =
    { name : string
    ; url : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Info = struct
  type t =
    { title : string
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; terms_of_service : string option [@jsonaf.option] [@sexp.option]
    ; contact : Contact.t option [@jsonaf.option] [@sexp.option]
    ; license : License.t option [@jsonaf.option] [@sexp.option]
    ; version : string
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Server_variable = struct
  type t =
    { enum : string list option [@jsonaf.option] [@sexp.option]
    ; default : string
    ; description : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Server = struct
  type t =
    { url : string
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; variables : Server_variable.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module External_documentation = struct
  type t =
    { description : string option [@jsonaf.option] [@sexp.option]
    ; url : string
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Tag = struct
  type t =
    { name : string
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; external_docs : External_documentation.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Security_requirement = struct
  type t = string list Jsonaf_string_map.t [@@deriving jsonaf, sexp_of]
end

module Example = struct
  type t =
    { summary : string option [@jsonaf.option] [@sexp.option]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; value : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; external_value : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Link = struct
  type t =
    { operation_ref : string option [@jsonaf.option] [@sexp.option]
    ; operation_id : string option [@jsonaf.option] [@sexp.option]
    ; parameters : Jsonaf.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; request_body : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; server : Server.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Discriminator = struct
  type t =
    { property_name : string
    ; mapping : string Jsonaf_string_map.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module XML = struct
  type t =
    { name : string option [@jsonaf.option] [@sexp.option]
    ; namespace : string option [@jsonaf.option] [@sexp.option]
    ; prefix : string option [@jsonaf.option] [@sexp.option]
    ; attribute : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; wrapped : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Schema = struct
  module Type = struct
    type t =
      | Array
      | Boolean
      | Integer
      | Null
      | Number
      | Object
      | String
      | Unable_to_parse of string [@fallback]
    [@@deriving string ~capitalize:"lowercase", compare, sexp_of]

    include Jsonaf.Jsonafable.Of_stringable (struct
        type nonrec t = t [@@deriving string]
      end)
  end

  module Format = struct
    type t =
      | Base64url
      | Binary
      | Byte
      | Char
      | Commonmark
      | Date_time_local
      | Date_time
      | Date
      | Decimal
      | Decimal128
      | Double_int
      | Double
      | Duration
      | Email
      | Float
      | Hostname
      | Html
      | Http_date
      | Idn_email
      | Idn_hostname
      | Int16
      | Int32
      | Int64
      | Int8
      | Ipv4
      | Ipv6
      | Iri_reference
      | Iri
      | Json_pointer
      | Media_range
      | Password
      | Regex
      | Relative_json_pointer
      | Sf_binary
      | Sf_boolean
      | Sf_decimal
      | Sf_integer
      | Sf_string
      | Sf_token
      | Time_local
      | Time
      | Uint16
      | Uint32
      | Uint64
      | Uint8
      | Unixtime
      | Uri_reference
      | Uri_template
      | Uri
      | Uuid
      | Unable_to_parse of string [@fallback]
    [@@deriving string ~capitalize:"kebab-case", compare, sexp_of]

    include Jsonaf.Jsonafable.Of_stringable (struct
        type nonrec t = t [@@deriving string]
      end)
  end

  type 'a additional_properties =
    [ `Not_allowed
    | `Allowed of 'a
    ]
  [@@deriving sexp_of]

  let jsonaf_of_additional_properties jsonaf_of_schema = function
    | `Allowed schema -> jsonaf_of_schema schema
    | `Not_allowed -> `False
  ;;

  let additional_properties_of_jsonaf schema_of_jsonaf = function
    | `True -> `Allowed (schema_of_jsonaf (`Object []))
    | `False -> `Not_allowed
    | value -> `Allowed (schema_of_jsonaf value)
  ;;

  type t =
    { title : string option [@jsonaf.option] [@sexp.option]
    ; multiple_of : float option [@jsonaf.option] [@sexp.option]
    ; maximum : float option [@jsonaf.option] [@sexp.option]
    ; exclusive_maximum : bool option [@jsonaf.option] [@sexp.option]
    ; minimum : float option [@jsonaf.option] [@sexp.option]
    ; exclusive_minimum : bool option [@jsonaf.option] [@sexp.option]
    ; max_length : int option [@jsonaf.option] [@sexp.option]
    ; min_length : int option [@jsonaf.option] [@sexp.option]
    ; pattern : string option [@jsonaf.option] [@sexp.option]
    ; max_items : int option [@jsonaf.option] [@sexp.option]
    ; min_items : int option [@jsonaf.option] [@sexp.option]
    ; unique_items : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; max_properties : int option [@jsonaf.option] [@sexp.option]
    ; min_properties : int option [@jsonaf.option] [@sexp.option]
    ; required : string list [@jsonaf.list] [@sexp.list]
    ; enum : Jsonaf.t list option [@jsonaf.option] [@sexp.option]
    ; type_ : Type.t option [@jsonaf.option] [@sexp.option] [@key "type"]
    ; all_of : t Or_reference.t list option [@jsonaf.option] [@sexp.option]
    ; one_of : t Or_reference.t list option [@jsonaf.option] [@sexp.option]
    ; any_of : t Or_reference.t list option [@jsonaf.option] [@sexp.option]
    ; not_ : t Or_reference.t option [@jsonaf.option] [@sexp.option] [@key "not"]
    ; items : t Or_reference.t option [@jsonaf.option] [@sexp.option]
    ; properties : t Or_reference.t Jsonaf_string_map.t option
         [@jsonaf.option] [@sexp.option]
    ; additional_properties : t Or_reference.t additional_properties option
         [@jsonaf.option] [@sexp.option]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; format : Format.t option [@jsonaf.option] [@sexp.option]
    ; default : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; nullable : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; discriminator : Discriminator.t option [@jsonaf.option] [@sexp.option]
    ; read_only : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; write_only : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; xml : XML.t option [@jsonaf.option] [@sexp.option]
    ; external_docs : External_documentation.t option [@jsonaf.option] [@sexp.option]
    ; example : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; deprecated : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
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
  [@@deriving jsonaf, sexp_of]
end = struct
  type t =
    { description : string option [@jsonaf.option] [@sexp.option]
    ; required : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; deprecated : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; allow_empty_value : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; style : string option [@jsonaf.option] [@sexp.option]
    ; explode : bool option [@jsonaf.option] [@sexp.option]
    ; allow_reserved : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; schema : Schema.t Or_reference.t option [@jsonaf.option] [@sexp.option]
    ; example : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; examples : Example.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@sexp.omit_nil]
    ; content : Media_type.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@sexp.omit_nil]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

and Encoding : sig
  type t =
    { content_type : string option
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
    ; style : string option
    ; explode : bool option
    ; allow_reserved : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end = struct
  type t =
    { content_type : string option [@jsonaf.option] [@sexp.option]
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; style : string option [@jsonaf.option] [@sexp.option]
    ; explode : bool option [@jsonaf.option] [@sexp.option]
    ; allow_reserved : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

and Media_type_or_server_sent_event : sig
  type t =
    | Server_sent_event of Media_type.t
    | Media_type of Media_type.t
  [@@deriving jsonaf, sexp_of]

  val media_type : t -> Media_type.t
end = struct
  type t =
    | Server_sent_event of Media_type.t
    | Media_type of Media_type.t
  [@@deriving sexp_of]

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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end = struct
  type t =
    { schema : Schema.t Or_reference.t option [@jsonaf.option] [@sexp.option]
    ; example : Jsonaf.t option [@jsonaf.option] [@sexp.option]
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; encoding : Encoding.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Parameter = struct
  module Jsonable = struct
    module In = struct
      type t =
        | Query
        | Header
        | Path
        | Cookie
      [@@deriving string ~capitalize:"lowercase"]

      include Jsonaf.Jsonafable.Of_stringable (struct
          type nonrec t = t [@@deriving string]
        end)
    end

    type t =
      { name : string
      ; in_ : In.t [@jsonaf.key "in"]
      ; style : string option [@jsonaf.option]
      ; description : string option [@jsonaf.option]
      ; required : bool [@default false] [@jsonaf_drop_default.equal]
      ; deprecated : bool [@default false] [@jsonaf_drop_default.equal]
      ; example : Jsonaf.t option [@jsonaf.option]
      ; examples : Example.t Or_reference.t Jsonaf_string_map.t
           [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf]
      ; explode : bool option [@jsonaf.option]
      ; schema : Schema.t Or_reference.t option [@jsonaf.option]
      ; content : Media_type.t Jsonaf_string_map.t option [@jsonaf.option]
      ; allow_reserved : bool option [@jsonaf.option]
      ; allow_empty_value : bool option [@jsonaf.option]
      }
    [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]
  end

  module Query_style = struct
    type t =
      | Form
      | Space_delimited
      | Pipe_delimited
      | Deep_object
    [@@deriving string ~capitalize:"camelCase", sexp_of]
  end

  module Header_style = struct
    type t = Simple [@@deriving string ~capitalize:"lowercase", sexp_of]
  end

  module Path_style = struct
    type t =
      | Matrix
      | Label
      | Simple
    [@@deriving string ~capitalize:"lowercase", sexp_of]
  end

  module Cookie_style = struct
    type t = Form [@@deriving string ~capitalize:"lowercase", sexp_of]
  end

  module Parameter_schema = struct
    type t =
      | Directly_defined of Schema.t Or_reference.t
      | Via_media_type of
          { media_type : string
          ; content : Media_type.t
          }
    [@@deriving sexp_of, variants]
  end

  module In = struct
    type t =
      | Query of
          { style : Query_style.t
          ; allow_reserved : bool option [@sexp.option]
          ; allow_empty_value : bool option [@sexp.option]
          }
      | Header of { style : Header_style.t }
      | Path of { style : Path_style.t }
      | Cookie of { style : Cookie_style.t }
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; description : string option [@sexp.option]
    ; required : bool [@sexp.bool]
    ; deprecated : bool [@sexp.bool]
    ; in_ : In.t
    ; schema : Parameter_schema.t
    ; example : Jsonaf.t option [@sexp.option]
    ; examples : Example.t Or_reference.t String.Map.t [@sexp.omit_nil]
    ; explode : bool option [@sexp.option]
    }
  [@@deriving sexp_of, fields ~getters ~setters]

  let to_jsonafable
    { name; description; required; deprecated; in_; schema; example; examples; explode }
    =
    let (in_ : Jsonable.In.t), style, allow_reserved, allow_empty_value =
      match in_ with
      | Query { style; allow_reserved; allow_empty_value } ->
        Query, Query_style.to_string style, allow_reserved, allow_empty_value
      | Header { style } -> Header, Header_style.to_string style, None, None
      | Path { style } -> Path, Path_style.to_string style, None, None
      | Cookie { style } -> Cookie, Cookie_style.to_string style, None, None
    in
    let content, schema =
      match schema with
      | Directly_defined schema -> None, Some schema
      | Via_media_type { media_type; content } ->
        Some (String.Map.singleton media_type content), None
    in
    { Jsonable.name
    ; in_
    ; style = Some style
    ; description
    ; required
    ; deprecated
    ; example
    ; examples
    ; explode
    ; schema
    ; content
    ; allow_reserved
    ; allow_empty_value
    }
  ;;

  let of_jsonafable
    { Jsonable.name
    ; in_
    ; style
    ; description
    ; required
    ; deprecated
    ; example
    ; examples
    ; explode
    ; schema
    ; content
    ; allow_reserved
    ; allow_empty_value
    }
    =
    let in_ : In.t =
      match in_ with
      | Query ->
        Query
          { style =
              Option.value_map style ~default:Query_style.Form ~f:Query_style.of_string
          ; allow_reserved
          ; allow_empty_value
          }
      | Header ->
        Header
          { style =
              Option.value_map
                style
                ~default:Header_style.Simple
                ~f:Header_style.of_string
          }
      | Path ->
        Path
          { style =
              Option.value_map style ~default:Path_style.Simple ~f:Path_style.of_string
          }
      | Cookie ->
        Cookie
          { style =
              Option.value_map style ~default:Cookie_style.Form ~f:Cookie_style.of_string
          }
    in
    let schema : Parameter_schema.t =
      match schema, Option.value_map ~default:[] ~f:Map.to_alist content with
      | _, _ :: _ :: _ ->
        raise
          (Openapi_spec_violation
             [%string
               "Invalid parameter %{name}: content field specifies more than one media \
                type"])
      | None, [] | Some _, [ _ ] ->
        raise
          (Openapi_spec_violation
             [%string
               "Invalid parameter %{name}: parameters must specify either content or \
                schema"])
      | Some schema, [] -> Directly_defined schema
      | None, [ (media_type, content) ] -> Via_media_type { media_type; content }
    in
    { name; description; required; deprecated; in_; schema; example; examples; explode }
  ;;

  include
    Jsonaf.Jsonafable.Of_jsonafable
      (Jsonable)
      (struct
        type nonrec t = t

        let to_jsonafable = to_jsonafable
        let of_jsonafable = of_jsonafable
      end)
end

module Request_body = struct
  type t =
    { description : string option [@jsonaf.option] [@sexp.option]
    ; content : Media_type.t Jsonaf_string_map.t
    ; required : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
  [@@jsonaf.allow_extra_fields]
end

module Response = struct
  type t =
    { description : string
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; content : Media_type_or_server_sent_event.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; links : Link.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Responses = struct
  type t = Response.t Or_reference.t Jsonaf_string_map.t [@@deriving jsonaf, sexp_of]
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end = struct
  type t =
    { tags : string list [@jsonaf.list] [@sexp.list]
    ; summary : string option [@jsonaf.option] [@sexp.option]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; external_docs : External_documentation.t option [@jsonaf.option] [@sexp.option]
    ; operation_id : string option [@jsonaf.option] [@sexp.option]
    ; parameters : Parameter.t Or_reference.t list [@jsonaf.list] [@sexp.list]
    ; request_body : Request_body.t Or_reference.t option [@jsonaf.option] [@sexp.option]
    ; responses : Responses.t
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; deprecated : bool [@default false] [@jsonaf_drop_default.equal] [@sexp.bool]
    ; security : Security_requirement.t list option [@jsonaf.option] [@sexp.option]
    ; servers : Server.t list [@jsonaf.list] [@sexp.list]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]

  val all_operations : t -> (Httpaf.Method.t * Operation.t) list
end = struct
  type t =
    { ref_ : string option [@key "$ref"] [@jsonaf.option] [@sexp.option]
    ; summary : string option [@jsonaf.option] [@sexp.option]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; get : Operation.t option [@jsonaf.option] [@sexp.option]
    ; put : Operation.t option [@jsonaf.option] [@sexp.option]
    ; post : Operation.t option [@jsonaf.option] [@sexp.option]
    ; delete : Operation.t option [@jsonaf.option] [@sexp.option]
    ; options : Operation.t option [@jsonaf.option] [@sexp.option]
    ; head : Operation.t option [@jsonaf.option] [@sexp.option]
    ; patch : Operation.t option [@jsonaf.option] [@sexp.option]
    ; trace : Operation.t option [@jsonaf.option] [@sexp.option]
    ; servers : Server.t list [@jsonaf.list] [@sexp.list]
    ; parameters : Parameter.t Or_reference.t list [@jsonaf.list] [@sexp.list]
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
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
  type t = Path_item.t [@@deriving jsonaf, sexp_of]
end = struct
  type t = Path_item.t [@@deriving jsonaf, sexp_of]
end

module Paths = struct
  type t = Path_item.t Jsonaf_string_map.t [@@deriving jsonaf, sexp_of]
end

module Oauth_flow = struct
  type t =
    { authorization_url : string option [@jsonaf.option] [@sexp.option]
    ; token_url : string option [@jsonaf.option] [@sexp.option]
    ; refresh_url : string option [@jsonaf.option] [@sexp.option]
    ; scopes : string Jsonaf_string_map.t
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Oauth_flows = struct
  type t =
    { implicit : Oauth_flow.t option [@jsonaf.option] [@sexp.option]
    ; password : Oauth_flow.t option [@jsonaf.option] [@sexp.option]
    ; client_credentials : Oauth_flow.t option [@jsonaf.option] [@sexp.option]
    ; authorization_code : Oauth_flow.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Security_scheme = struct
  type t =
    { type_ : string [@key "type"]
    ; description : string option [@jsonaf.option] [@sexp.option]
    ; name : string option [@jsonaf.option] [@sexp.option]
    ; in_ : string option [@key "in"] [@jsonaf.option] [@sexp.option]
    ; scheme : string option [@jsonaf.option] [@sexp.option]
    ; bearer_format : string option [@jsonaf.option] [@sexp.option]
    ; flows : Oauth_flows.t option [@jsonaf.option] [@sexp.option]
    ; open_id_connect_url : string option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]
end

module Components = struct
  type t =
    { schemas : Schema.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; responses : Response.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; parameters : Parameter.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; request_bodies : Request_body.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; security_schemes : Security_scheme.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; links : Link.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
         [@default Jsonaf_string_map.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]

  let empty = t_of_jsonaf (`Object [])
end

module Open_api = struct
  type t =
    { openapi : string
    ; info : Info.t
    ; servers : Server.t list option [@jsonaf.option] [@sexp.option]
    ; paths : Paths.t
    ; components : Components.t
         [@default Components.empty] [@jsonaf_drop_default.jsonaf] [@sexp.omit_nil]
    ; security : Security_requirement.t list option [@jsonaf.option] [@sexp.option]
    ; tags : Tag.t list option [@jsonaf.option] [@sexp.option]
    ; external_docs : External_documentation.t option [@jsonaf.option] [@sexp.option]
    }
  [@@deriving
    jsonaf ~capitalize:"camelCase", fields ~getters ~setters ~iterators:create, sexp_of]

  let t_of_jsonaf json =
    let open struct
      type just_the_version = { openapi : string }
      [@@deriving jsonaf] [@@jsonaf.allow_extra_fields]

      let with_version_numbers { openapi } = openapi, String.split ~on:'.' openapi
    end in
    try t_of_jsonaf json with
    | exn ->
      let backtrace = Backtrace.Exn.most_recent () in
      (match with_version_numbers (just_the_version_of_jsonaf json) with
       | _, [ "3"; ("0" | "1"); _ ] ->
         (* We should be able to support this, so we just reraise. *)
         Exn.raise_with_original_backtrace exn backtrace
       | exception _ ->
         (* Couldn't even get the version number, probably fed junk, just raise *)
         Exn.raise_with_original_backtrace exn backtrace
       | openapi_version, [ "3"; _; _ ] ->
         Exn.raise_with_original_backtrace
           (Exn.Reraised
              ( [%string
                  "The generator currently does not support features introduced after \
                   OpenAPI 3.1, reported spec version is %{openapi_version}, which may \
                   have caused us to fail to parse the spec."]
              , exn ))
           backtrace
       | openapi_version, _ ->
         Exn.raise_with_original_backtrace
           (Exn.Reraised
              ( [%string
                  "Unknown OpenAPI version number, only OpenAPI 3 is supported, but \
                   reported spec version is %{openapi_version}, which may have caused us \
                   to fail to parse the spec."]
              , exn )))
        backtrace
  ;;
end
