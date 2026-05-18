open! Core
open! Jsonaf.Export

exception Openapi_spec_violation of string

val combine_exn : exn -> exn -> exn

module Jsonaf_string_map : sig
  type 'a t = 'a String.Map.t [@@deriving jsonaf, sexp_of]
end

module Component_lookup_location : sig
  type t =
    | Parameters
    | Request_bodies
    | Responses
    | Schemas
end

module Reference : sig
  type 'a t = { ref_ : string }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of, compare]

  val last_segment : string -> string option
end

module Or_reference : sig
  type 'a t =
    | Ref of 'a Reference.t
    | Value of 'a
  [@@deriving jsonaf, sexp_of, compare]

  val value_exn : 'a t -> 'a
end

module Contact : sig
  type t =
    { name : string option
    ; url : string option
    ; email : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module License : sig
  type t =
    { name : string
    ; url : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Info : sig
  type t =
    { title : string
    ; description : string option
    ; terms_of_service : string option
    ; contact : Contact.t option
    ; license : License.t option
    ; version : string
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Server_variable : sig
  type t =
    { enum : string list option
    ; default : string
    ; description : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Server : sig
  type t =
    { url : string
    ; description : string option
    ; variables : Server_variable.t String.Map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module External_documentation : sig
  type t =
    { description : string option
    ; url : string
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Tag : sig
  type t =
    { name : string
    ; description : string option
    ; external_docs : External_documentation.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Security_requirement : sig
  type t = string list String.Map.t [@@deriving jsonaf]
end

module Example : sig
  type t =
    { summary : string option
    ; description : string option
    ; value : Jsonaf.t option
    ; external_value : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Link : sig
  type t =
    { operation_ref : string option
    ; operation_id : string option
    ; parameters : Jsonaf.t String.Map.t
    ; request_body : Jsonaf.t option
    ; description : string option
    ; server : Server.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Discriminator : sig
  type t =
    { property_name : string
    ; mapping : string String.Map.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module XML : sig
  type t =
    { name : string option
    ; namespace : string option
    ; prefix : string option
    ; attribute : bool
    ; wrapped : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Schema : sig
  module Type : sig
    type t =
      | Array
      | Boolean
      | Integer
      | Null
      | Number
      | Object
      | String
      | Unable_to_parse of string
    [@@deriving string, jsonaf, compare, sexp_of]
  end

  module Format : sig
    (** Formats from the OpenAPI Format Registry. See
        {{:https://spec.openapis.org/registry/format/} OpenAPI Format Registry}.

        This list might drift out of date, and not all the formats are explicitly
        supported by the code generator. *)
    type t =
      | Base64url
      (** Binary data encoded as a url-safe string as defined in RFC4648. Compatible with:
          string. Deprecated. *)
      | Binary (** Any sequence of octets. Compatible with: string. Deprecated. *)
      | Byte
      (** Base64 encoded data as defined in RFC4648. Compatible with: string. Deprecated. *)
      | Char (** A single character. Compatible with: string. *)
      | Commonmark (** Commonmark-formatted text. Compatible with: string. *)
      | Date_time_local
      (** RFC3339 date-time without the timezone component. Compatible with: string. *)
      | Date_time
      (** Date and time as defined by date-time - RFC3339. Compatible with: string. *)
      | Date (** Date as defined by full-date - RFC3339. Compatible with: string. *)
      | Decimal
      (** A fixed point decimal number of unspecified precision and range. Compatible
          with: string, number. *)
      | Decimal128
      (** A decimal floating-point number with 34 significant decimal digits. Compatible
          with: string, number. *)
      | Double_int
      (** An integer that can be stored in an IEEE 754 double-precision number without
          loss of precision. Compatible with: number. *)
      | Double (** Double precision floating point number. Compatible with: number. *)
      | Duration
      (** Duration as defined by duration - RFC3339. Compatible with: string. *)
      | Email
      (** An email address as defined as Mailbox in RFC5321. Compatible with: string. *)
      | Float (** Single precision floating point number. Compatible with: number. *)
      | Hostname (** A host name as defined by RFC1123. Compatible with: string. *)
      | Html (** HTML-formatted text. Compatible with: string. *)
      | Http_date
      (** Date and time as defined by HTTP-date - RFC7231. Compatible with: string. *)
      | Idn_email
      (** An email address as defined as Mailbox in RFC6531. Compatible with: string. *)
      | Idn_hostname
      (** An internationalized host name as defined by RFC5890. Compatible with: string. *)
      | Int16 (** Signed 16-bit integer. Compatible with: number. *)
      | Int32 (** Signed 32-bit integer. Compatible with: number. *)
      | Int64 (** Signed 64-bit integer. Compatible with: number, string. *)
      | Int8 (** Signed 8-bit integer. Compatible with: number. *)
      | Ipv4
      (** An IPv4 address as defined as dotted-quad by RFC2673. Compatible with: string. *)
      | Ipv6 (** An IPv6 address as defined by RFC4673. Compatible with: string. *)
      | Iri_reference
      (** An Internationalized Resource Identifier as defined in RFC3987. Compatible with:
          string. *)
      | Iri
      (** An Internationalized Resource Identifier as defined in RFC3987. Compatible with:
          string. *)
      | Json_pointer
      (** A JSON string representation of a JSON Pointer as defined in RFC6901. Compatible
          with: string. *)
      | Media_range
      (** A media type as defined by the media-range ABNF production in RFC9110.
          Compatible with: string. *)
      | Password (** A string that hints to obscure the value. Compatible with: string. *)
      | Regex (** A regular expression as defined in ECMA-262. Compatible with: string. *)
      | Relative_json_pointer
      (** A JSON string representation of a relative JSON Pointer as defined in draft RFC
          01. Compatible with: string. *)
      | Sf_binary
      (** Structured fields byte sequence as defined in RFC8941. Compatible with: string. *)
      | Sf_boolean
      (** Structured fields boolean as defined in RFC8941. Compatible with: string. *)
      | Sf_decimal
      (** Structured fields decimal as defined in RFC8941. Compatible with: number. *)
      | Sf_integer
      (** Structured fields integer as defined in RFC8941. Compatible with: number. *)
      | Sf_string
      (** Structured fields string as defined in RFC8941. Compatible with: string. *)
      | Sf_token
      (** Structured fields token as defined in RFC8941. Compatible with: string. *)
      | Time_local
      (** RFC3339 time without the timezone component. Compatible with: string. *)
      | Time (** Time as defined by full-time - RFC3339. Compatible with: string. *)
      | Uint16 (** Unsigned 16-bit integer. Compatible with: number. *)
      | Uint32 (** Unsigned 32-bit integer. Compatible with: number. *)
      | Uint64 (** Unsigned 64-bit integer. Compatible with: number, string. *)
      | Uint8 (** Unsigned 8-bit integer. Compatible with: number. *)
      | Unixtime
      (** Seconds since Jan 1st 1970 - IEEE1003.1-2024/POSIX.1-2024. Compatible with:
          number, string. *)
      | Uri_reference
      (** A URI reference as defined in RFC3986. Compatible with: string. *)
      | Uri_template (** A URI Template as defined in RFC6570. Compatible with: string. *)
      | Uri
      (** A Uniform Resource Identifier as defined in RFC3986. Compatible with: string. *)
      | Uuid
      (** A Universally Unique IDentifier as defined in RFC4122. Compatible with: string. *)
      | Unable_to_parse of string
    [@@deriving string, jsonaf, compare, sexp_of]
  end

  type t =
    { title : string option
    ; multiple_of : float option
    ; maximum : float option
    ; exclusive_maximum : bool option
    ; minimum : float option
    ; exclusive_minimum : bool option
    ; max_length : int option
    ; min_length : int option
    ; pattern : string option
    ; max_items : int option
    ; min_items : int option
    ; unique_items : bool
    ; max_properties : int option
    ; min_properties : int option
    ; required : string list
    ; enum : Jsonaf.t list option
    ; type_ : Type.t option
    ; all_of : t Or_reference.t list option
    ; one_of : t Or_reference.t list option
    ; any_of : t Or_reference.t list option
    ; not_ : t Or_reference.t option
    ; items : t Or_reference.t option
    ; properties : t Or_reference.t String.Map.t option
    ; additional_properties : [ `Allowed of t Or_reference.t | `Not_allowed ] option
    ; description : string option
    ; format : Format.t option
    ; default : Jsonaf.t option
    ; nullable : bool
    ; discriminator : Discriminator.t option
    ; read_only : bool
    ; write_only : bool
    ; xml : XML.t option
    ; external_docs : External_documentation.t option
    ; example : Jsonaf.t option
    ; deprecated : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
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
    ; examples : Example.t String.Map.t
    ; content : Media_type.t String.Map.t
    }
  [@@deriving jsonaf]
end

and Encoding : sig
  type t =
    { content_type : string option
    ; headers : Header.t Or_reference.t String.Map.t
    ; style : string option
    ; explode : bool option
    ; allow_reserved : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

and Media_type_or_server_sent_event : sig
  type t =
    | Server_sent_event of Media_type.t
    | Media_type of Media_type.t
  [@@deriving jsonaf]

  val media_type : t -> Media_type.t
end

and Media_type : sig
  type t =
    { schema : Schema.t Or_reference.t option
    ; example : Jsonaf.t option
    ; examples : Example.t Or_reference.t String.Map.t
    ; encoding : Encoding.t String.Map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Parameter : sig
  module Query_style : sig
    type t =
      | Form
      | Space_delimited
      | Pipe_delimited
      | Deep_object
    [@@deriving string, sexp_of]
  end

  module Header_style : sig
    type t = Simple [@@deriving string, sexp_of]
  end

  module Path_style : sig
    type t =
      | Matrix
      | Label
      | Simple
    [@@deriving string, sexp_of]
  end

  module Cookie_style : sig
    type t = Form [@@deriving string, sexp_of]
  end

  module Parameter_schema : sig
    type t =
      | Directly_defined of Schema.t Or_reference.t
      | Via_media_type of
          { media_type : string
          ; content : Media_type.t
          }
    [@@deriving sexp_of, variants]
  end

  module In : sig
    type t =
      | Query of
          { style : Query_style.t
          ; allow_reserved : bool option
          ; allow_empty_value : bool option
          }
      | Header of { style : Header_style.t }
      | Path of { style : Path_style.t }
      | Cookie of { style : Cookie_style.t }
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; description : string option
    ; required : bool
    ; deprecated : bool
    ; in_ : In.t
    ; schema : Parameter_schema.t
    ; example : Jsonaf.t option
    ; examples : Example.t Or_reference.t String.Map.t
    ; explode : bool option
    }
  [@@deriving jsonaf, sexp_of, fields ~getters ~setters]
end

module Request_body : sig
  type t =
    { description : string option
    ; content : Media_type.t String.Map.t
    ; required : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Response : sig
  type t =
    { description : string
    ; headers : Header.t Or_reference.t String.Map.t
    ; content : Media_type_or_server_sent_event.t String.Map.t
    ; links : Link.t Or_reference.t String.Map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Responses : sig
  type t = Response.t Or_reference.t String.Map.t [@@deriving jsonaf, sexp_of]
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
    ; callbacks : Callback.t Or_reference.t String.Map.t
    ; deprecated : bool
    ; security : Security_requirement.t list option
    ; servers : Server.t list
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
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
end

and Callback : sig
  type t = Path_item.t [@@deriving jsonaf]
end

module Paths : sig
  type t = Path_item.t String.Map.t [@@deriving jsonaf]
end

module Oauth_flow : sig
  type t =
    { authorization_url : string option
    ; token_url : string option
    ; refresh_url : string option
    ; scopes : string String.Map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Oauth_flows : sig
  type t =
    { implicit : Oauth_flow.t option
    ; password : Oauth_flow.t option
    ; client_credentials : Oauth_flow.t option
    ; authorization_code : Oauth_flow.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Security_scheme : sig
  type t =
    { type_ : string
    ; description : string option
    ; name : string option
    ; in_ : string option
    ; scheme : string option
    ; bearer_format : string option
    ; flows : Oauth_flows.t option
    ; open_id_connect_url : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Components : sig
  type t =
    { schemas : Schema.t Or_reference.t String.Map.t
    ; responses : Response.t Or_reference.t String.Map.t
    ; parameters : Parameter.t Or_reference.t String.Map.t
    ; examples : Example.t Or_reference.t String.Map.t
    ; request_bodies : Request_body.t Or_reference.t String.Map.t
    ; headers : Header.t Or_reference.t String.Map.t
    ; security_schemes : Security_scheme.t Or_reference.t String.Map.t
    ; links : Link.t Or_reference.t String.Map.t
    ; callbacks : Callback.t Or_reference.t String.Map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end

module Open_api : sig
  type t =
    { openapi : string
    ; info : Info.t
    ; servers : Server.t list option
    ; paths : Paths.t
    ; components : Components.t
    ; security : Security_requirement.t list option
    ; tags : Tag.t list option
    ; external_docs : External_documentation.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create, sexp_of]
end
