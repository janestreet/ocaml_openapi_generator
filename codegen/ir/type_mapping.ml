open! Core

module Matcher = struct
  module Type = struct
    include Openapi_spec.Types.Schema.Type
    include Sexpable.Of_stringable (Openapi_spec.Types.Schema.Type)

    let t_sexp_grammar =
      { Sexplib.Sexp_grammar.untyped = String }
      |> Sexplib.Sexp_grammar.tag
           ~key:Sexplib.Sexp_grammar.type_name_tag
           ~value:[%message "Type.t"]
      |> Sexplib.Sexp_grammar.tag
           ~key:Sexplib.Sexp_grammar.doc_comment_tag
           ~value:
             [%message
               "The type of the schema per the OpenAPI spec (e.g. number, integer, \
                string)."]
    ;;
  end

  module Format = struct
    include Openapi_spec.Types.Schema.Format
    include Sexpable.Of_stringable (Openapi_spec.Types.Schema.Format)

    let t_sexp_grammar =
      { Sexplib.Sexp_grammar.untyped = String }
      |> Sexplib.Sexp_grammar.tag
           ~key:Sexplib.Sexp_grammar.type_name_tag
           ~value:[%message "Format.t"]
      |> Sexplib.Sexp_grammar.tag
           ~key:Sexplib.Sexp_grammar.doc_comment_tag
           ~value:
             [%message "The format of the schema per the OpenAPI spec (e.g. ipv4, uuid)."]
    ;;
  end

  type t =
    | Type of Type.t
    | Format of Format.t
  [@@deriving sexp, sexp_grammar]
end

type t =
  { matches : Matcher.t Blang.t
  ; description : Type_description.t
  }
[@@deriving sexp, sexp_grammar]

let create_preset ?format ~type_ description =
  { matches =
      List.filter_opt
        [ Some (Blang.base (Matcher.Type type_))
        ; Option.map format ~f:(fun format -> Blang.base (Matcher.Format format))
        ]
      |> Blang.and_
  ; description
  }
;;

let presets =
  let open Type_description.Presets in
  [ create_preset ~type_:Integer ~format:Int64 int64
  ; create_preset ~type_:Integer int
  ; create_preset ~type_:Number ~format:Double float
  ; create_preset ~type_:Boolean bool
  ; create_preset
      ~type_:String
      ~format:(Matcher.Format.of_string "ip" (* Non standard *))
      ip
  ; create_preset ~type_:String ~format:Ipv4 ipv4
  ; create_preset ~type_:String ~format:Ipv6 ipv6
  ; create_preset ~type_:String ~format:Date_time time
  ; create_preset ~type_:String ~format:Uuid uuid
  ; create_preset ~type_:String string
  ]
;;
