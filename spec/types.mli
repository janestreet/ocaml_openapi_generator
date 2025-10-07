open! Core
open! Jsonaf.Export

exception Openapi_spec_violation of string

val combine_exn : exn -> exn -> exn

module Jsonaf_string_map : sig
  include module type of String.Map

  val jsonaf_of_t : ('a -> Jsonaf.t) -> 'a t -> Jsonaf.t
  val t_of_jsonaf : (Jsonaf.t -> 'a) -> Jsonaf.t -> 'a t
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]

  val last_segment : string -> string option
end

module Or_reference : sig
  type 'a t =
    | Ref of 'a Reference.t
    | Value of 'a

  val t_of_jsonaf : (Jsonaf_kernel.t -> 'a) -> Jsonaf_kernel.t -> 'a t
  val jsonaf_of_t : ('a -> Jsonaf_kernel.t) -> 'a t -> Jsonaf_kernel.t
  val value_exn : 'a t -> 'a
end

module Contact : sig
  type t =
    { name : string option
    ; url : string option
    ; email : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module License : sig
  type t =
    { name : string
    ; url : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Server_variable : sig
  type t =
    { enum : string list option
    ; default : string
    ; description : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Server : sig
  type t =
    { url : string
    ; description : string option
    ; variables : Server_variable.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module External_documentation : sig
  type t =
    { description : string option
    ; url : string
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Tag : sig
  type t =
    { name : string
    ; description : string option
    ; external_docs : External_documentation.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Security_requirement : sig
  type t = string list Jsonaf_string_map.t [@@deriving jsonaf]
end

module Example : sig
  type t =
    { summary : string option
    ; description : string option
    ; value : Jsonaf.t option
    ; external_value : string option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Link : sig
  type t =
    { operation_ref : string option
    ; operation_id : string option
    ; parameters : Jsonaf.t Jsonaf_string_map.t
    ; request_body : Jsonaf.t option
    ; description : string option
    ; server : Server.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Discriminator : sig
  type t =
    { property_name : string
    ; mapping : string Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module XML : sig
  type t =
    { name : string option
    ; namespace : string option
    ; prefix : string option
    ; attribute : bool
    ; wrapped : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Schema : sig
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
    ; type_ : string option
    ; all_of : t Or_reference.t list option
    ; one_of : t Or_reference.t list option
    ; any_of : t Or_reference.t list option
    ; not_ : t Or_reference.t option
    ; items : t Or_reference.t option
    ; properties : t Or_reference.t Jsonaf_string_map.t option
    ; additional_properties : Jsonaf.t option
    ; description : string option
    ; format : string option
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
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
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
    ; encoding : Encoding.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Parameter : sig
  module Parameter_data : sig
    type t =
      { name : string
      ; description : string option
      ; required : bool
      ; deprecated : bool
      ; example : Jsonaf.t option
      ; examples : Example.t Or_reference.t Jsonaf_string_map.t
      ; explode : bool option
      ; schema : Schema.t Or_reference.t option
      ; content : Media_type.t Jsonaf_string_map.t option
      ; allow_reserved : bool option
      ; allow_empty_value : bool option
      }
    [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
  end

  module Query_style : sig
    type t =
      | Form
      | Space_delimited
      | Pipe_delimited
      | Deep_object
    [@@deriving jsonaf]
  end

  module Header_style : sig
    type t = Simple [@@deriving jsonaf]
  end

  module Path_style : sig
    type t =
      | Matrix
      | Label
      | Simple
    [@@deriving jsonaf]
  end

  module Cookie_style : sig
    type t = Form [@@deriving jsonaf]
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
  [@@deriving jsonaf]

  val get_style : (string * Jsonaf.t) list -> Jsonaf.t
  val data : t -> Parameter_data.t
end

module Request_body : sig
  type t =
    { description : string option
    ; content : Media_type.t Jsonaf_string_map.t
    ; required : bool
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Response : sig
  type t =
    { description : string
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
    ; content : Media_type_or_server_sent_event.t Jsonaf_string_map.t
    ; links : Link.t Or_reference.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Responses : sig
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
end

and Callback : sig
  type t = Path_item.t [@@deriving jsonaf]
end

module Paths : sig
  type t = Path_item.t Jsonaf_string_map.t [@@deriving jsonaf]
end

module Oauth_flow : sig
  type t =
    { authorization_url : string option
    ; token_url : string option
    ; refresh_url : string option
    ; scopes : string Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Oauth_flows : sig
  type t =
    { implicit : Oauth_flow.t option
    ; password : Oauth_flow.t option
    ; client_credentials : Oauth_flow.t option
    ; authorization_code : Oauth_flow.t option
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end

module Components : sig
  type t =
    { schemas : Schema.t Or_reference.t Jsonaf_string_map.t
    ; responses : Response.t Or_reference.t Jsonaf_string_map.t
    ; parameters : Parameter.t Or_reference.t Jsonaf_string_map.t
    ; examples : Example.t Or_reference.t Jsonaf_string_map.t
    ; request_bodies : Request_body.t Or_reference.t Jsonaf_string_map.t
    ; headers : Header.t Or_reference.t Jsonaf_string_map.t
    ; security_schemes : Security_scheme.t Or_reference.t Jsonaf_string_map.t
    ; links : Link.t Or_reference.t Jsonaf_string_map.t
    ; callbacks : Callback.t Or_reference.t Jsonaf_string_map.t
    }
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
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
  [@@deriving jsonaf, fields ~getters ~setters ~iterators:create]
end
