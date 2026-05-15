open! Core

let validate_ocaml_identifier_exn s =
  if String.is_empty s
  then raise_s [%message "OCaml identifiers or module names cannot be empty"];
  if not (Char.is_alpha s.[0])
  then
    raise_s
      [%message
        "OCaml identifier and module names must start with an alphabetical character" s];
  if not
       (String.for_all s ~f:(function
         | '\'' | '_' -> true
         | c -> Char.is_alphanum c))
  then
    raise_s
      [%message
        "OCaml identifiers and module names can only contain alphanumerical characters, \
         _, and '."
          s]
;;

module Namespace = struct
  (* We only support one argument per functor, and we don't support applicative functors
  *)
  type t = [ `Functor_application of string * t | `Module of string ] Nonempty_list.t
  [@@deriving compare]

  let of_string namespace =
    let parts =
      String.split ~on:'.' namespace
      |> List.intersperse ~sep:"."
      |> List.concat_map ~f:(fun string ->
        String.split ~on:'(' string
        |> List.intersperse ~sep:"("
        |> List.concat_map ~f:(fun string ->
          String.split ~on:')' string |> List.intersperse ~sep:")"))
      |> List.filter ~f:(fun string -> not (String.is_empty string))
    in
    let validate_module_name module_name =
      validate_ocaml_identifier_exn module_name;
      if Char.is_lowercase module_name.[0]
      then raise_s [%message "Module names must start with an uppercase" module_name]
    in
    let rec of_parts parts ~nested =
      match parts with
      | module_name :: "(" :: rest ->
        validate_module_name module_name;
        let parsed, rest = of_parts rest ~nested:true in
        of_parts_chain (`Functor_application (module_name, parsed)) rest ~nested
      | module_name :: rest ->
        validate_module_name module_name;
        of_parts_chain (`Module module_name) rest ~nested
      | [] -> raise_s [%message "Invalid namespace" namespace (parts : string list)]
    and of_parts_chain hd rest ~nested =
      match rest with
      | [] ->
        if nested
        then raise_s [%message "Invalid namespace, unmatched parentheses" namespace]
        else Nonempty_list.return hd, []
      | "." :: rest ->
        let parts, rest = of_parts rest ~nested in
        Nonempty_list.cons hd parts, rest
      | ")" :: rest ->
        if nested
        then Nonempty_list.return hd, rest
        else raise_s [%message "Invalid namespace, unmatched parentheses" namespace]
      | _ -> raise_s [%message "Invalid namespace" namespace (parts : string list)]
    in
    let t, rest = of_parts parts ~nested:false in
    assert (List.is_empty rest);
    t
  ;;

  let rec to_string t =
    Nonempty_list.to_list t
    |> List.map ~f:(function
      | `Module name -> name
      | `Functor_application (name, application) ->
        [%string "%{name}(%{to_string application})"])
    |> String.concat ~sep:"."
  ;;

  include Sexpable.Of_stringable (struct
      type nonrec t = t [@@deriving string]
    end)

  let namespace_description =
    "Namespace must be a valid OCaml path. Only supports applicative functors with \
     single arguments."
  ;;

  let t_sexp_grammar =
    { Sexplib.Sexp_grammar.untyped = String }
    |> Sexplib.Sexp_grammar.tag
         ~key:Sexplib.Sexp_grammar.type_name_tag
         ~value:[%message "Namespace.t"]
    |> Sexplib.Sexp_grammar.tag
         ~key:Sexplib.Sexp_grammar.doc_comment_tag
         ~value:[%message namespace_description]
  ;;

  include Comparable.Make_plain (struct
      type nonrec t = t [@@deriving compare, sexp]
    end)
end

module Identifier = struct
  type t =
    { namespace : Namespace.t option
    ; name : string
    }
  [@@deriving compare]

  let of_string identifier =
    let namespace, name =
      match String.rsplit2 identifier ~on:'.' with
      | Some (namespace, name) ->
        let namespace = Namespace.of_string namespace in
        Some namespace, name
      | None -> None, identifier
    in
    validate_ocaml_identifier_exn name;
    if Char.is_uppercase name.[0]
    then raise_s [%message "Identifiers must start with a lowercase" name];
    { namespace; name }
  ;;

  let to_string { namespace; name } =
    match namespace with
    | None -> name
    | Some namespace -> [%string "%{namespace#Namespace}.%{name}"]
  ;;

  include Sexpable.Of_stringable (struct
      type nonrec t = t [@@deriving string]
    end)

  let t_sexp_grammar =
    { Sexplib.Sexp_grammar.untyped = String }
    |> Sexplib.Sexp_grammar.tag
         ~key:Sexplib.Sexp_grammar.type_name_tag
         ~value:[%message "Identifier.t"]
    |> Sexplib.Sexp_grammar.tag
         ~key:Sexplib.Sexp_grammar.doc_comment_tag
         ~value:
           [%message
             [%string
               "A valid OCaml identifier. May be qualified with a namespace \
                (%{Namespace.namespace_description})."]]
  ;;

  include Comparable.Make_plain (struct
      type nonrec t = t [@@deriving compare, sexp]
    end)
end

type t =
  { name : Identifier.t
  ; arguments : t list [@sexp.list]
  ; needs_string_primitives : bool [@sexp.bool]
  }
[@@deriving sexp, sexp_grammar, equal]

let create ?(arguments = []) ?(needs_string_primitives = false) name =
  { name; arguments; needs_string_primitives }
;;

let generated name ~maximum_filename_length =
  { name =
      { namespace =
          Some
            (Namespace.of_string
               (Name.to_module_name name ~kind:(`Truncated maximum_filename_length)))
      ; name = "t"
      }
  ; needs_string_primitives = false
  ; arguments = []
  }
;;

let create_helper ?arguments ?needs_string_primitives name =
  create (Identifier.of_string name) ?needs_string_primitives ?arguments
;;

let option t = create_helper "option" ~arguments:[ t ]
let list t = create_helper "list" ~arguments:[ t ]

let string_assoc t =
  create_helper ~arguments:[ t ] "Openapi_runtime.Jane_with_json.Assoc.M(Core.String).t"
;;

let rec type_name { name; arguments; _ } =
  match arguments with
  | [] -> Identifier.to_string name
  | [ argument ] -> [%string "%{type_name argument} %{name#Identifier}"]
  | arguments ->
    let arguments = List.map arguments ~f:type_name |> String.concat ~sep:", " in
    [%string "(%{arguments}) %{name#Identifier}"]
;;

module Presets = struct
  let string =
    create_helper
      "string" (* doesn't need string primitives, it's just the identity function *)
  ;;

  let int = create_helper "int" ~needs_string_primitives:true
  let int64 = create_helper "int64" ~needs_string_primitives:true
  let float = create_helper "float" ~needs_string_primitives:true
  let bool = create_helper "bool" ~needs_string_primitives:true
  let jsonaf = create_helper "Jsonaf.With_structural_compare.t"
  let ip = create_helper "Openapi_runtime.Jane_with_json.Jsonaf_ip.t"
  let ipv4 = create_helper "Openapi_runtime.Jane_with_json.Jsonaf_ipv4.t"
  let ipv6 = create_helper "Openapi_runtime.Jane_with_json.Jsonaf_ipv6.t"
  let time = create_helper "Openapi_runtime.Jane_with_json.Jsonaf_time.t"
  let uuid = create_helper "Openapi_runtime.Jane_with_json.Jsonaf_uuid.t"
end
